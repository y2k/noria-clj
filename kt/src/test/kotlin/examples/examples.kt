package examples

import noria.*
import noria.LCS.updateOrder
import java.awt.Component
import java.awt.FlowLayout
import java.awt.LayoutManager
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

object Label : Reconciler<JLabel?, String, JLabel> {
    override fun Noria.reconcile(state: JLabel?, arg: String): Pair<JLabel, Thunk<JLabel>> {
        val newState = state ?: JLabel()
        newState.text = arg
        return newState to identity(newState)
    }
}


data class ButtonProps(var text: String,
                       var onClick: () -> Unit)

object Button : EntityReconciler<JButton, ButtonProps> {
    override fun Noria.construct(props: ButtonProps): JButton {
        val button = JButton(props.text)
        button.putClientProperty(ButtonProps::class, props)
        button.addActionListener {
            val props = button.getClientProperty(ButtonProps::class) as ButtonProps
            props.onClick()
        }
        return button
    }

    override fun Noria.reconcile(e: JButton, oldProps: ButtonProps, newProps: ButtonProps) {
        if (oldProps.text != newProps.text) {
            e.text = newProps.text
        }
        e.putClientProperty(ButtonProps::class, newProps)
    }
}

class OrderedPanelState(val panel: JPanel,
                        val props: OrderedPanelProps,
                        val components: Array<Component>)

data class OrderedPanelProps(val componentThunks: List<Thunk<Component>>,
                             val layoutManager: LayoutManager = FlowLayout())

object OrderedPanel : Reconciler<OrderedPanelState?, OrderedPanelProps, JPanel> {
    override fun Noria.reconcile(state: OrderedPanelState?,
                                 arg: OrderedPanelProps): Pair<OrderedPanelState?, Thunk<JPanel>> {
        val components = arg.componentThunks.map { x ->
            x.deref()
        }.toTypedArray()
        if (state == null || arg.layoutManager.javaClass != state.panel.layout.javaClass) {
            val panel = JPanel(arg.layoutManager)
            val newState = OrderedPanelState(panel, arg, components)
            components.forEach { x ->
                panel.add(x)
            }
            return newState to identity(panel)
        } else {
            val panel = state.panel

            updateOrder(state.components, components, object : LCS.OrderUpdater {
                override fun add(index: Int, value: Any?) {
                    panel.add(value as Component, index)
                }

                override fun remove(index: Int) {
                    panel.remove(index)
                }
            })
            return OrderedPanelState(panel, arg, components) to identity(panel)
        }
    }

    override fun destroy(state: OrderedPanelState?) {}
}

object Frame : EntityReconciler<JFrame, Thunk<JPanel>> {
    override fun Noria.construct(props: Thunk<JPanel>): JFrame {
        val frame = JFrame()
        frame.isVisible = true
        frame.rootPane.contentPane = props.deref()
        frame.pack()
        return frame
    }

    override fun Noria.reconcile(e: JFrame, oldProps: Thunk<JPanel>, newProps: Thunk<JPanel>) {
        e.rootPane.contentPane = newProps.deref()
    }
}

data class TextFieldProps(val onChange: (String) -> Unit,
                          val text: String)

object TextField : EntityReconciler<JTextField, TextFieldProps> {
    override fun Noria.construct(props: TextFieldProps): JTextField {
        val t = JTextField()
        t.text = props.text
        t.document.addDocumentListener(object : DocumentListener {
            override fun changedUpdate(e: DocumentEvent) {
                val props = t.getClientProperty(TextFieldProps::class) as TextFieldProps
                props.onChange(e.document.getText(0, e.document.length))
            }

            override fun insertUpdate(e: DocumentEvent) {
                val props = t.getClientProperty(TextFieldProps::class) as TextFieldProps
                props.onChange(e.document.getText(0, e.document.length))
            }

            override fun removeUpdate(e: DocumentEvent) {
                val props = t.getClientProperty(TextFieldProps::class) as TextFieldProps
                props.onChange(e.document.getText(0, e.document.length))
            }
        })
        t.putClientProperty(TextFieldProps::class, props)
        return t
    }

    override fun Noria.reconcile(e: JTextField, oldProps: TextFieldProps, newProps: TextFieldProps) {
        if (oldProps.text != newProps.text) {
            e.text = newProps.text
        }
        e.putClientProperty(TextFieldProps::class, newProps)
    }
}

fun Noria.flowPanel(builder: MutableList<Thunk<Component>>.() -> Unit): Thunk<JPanel> {
    val l = arrayListOf<Thunk<Component>>()
    l.builder()
    return OrderedPanel(OrderedPanelProps(l))
}

fun Noria.button(text: String, onClick: () -> Unit): Thunk<JButton> {
    return Button(ButtonProps(text = text, onClick = onClick))
}

fun Noria.textField(text: String, onChange: (String) -> Unit): Thunk<JTextField> {
    return TextField(TextFieldProps(onChange, text))
}

val i = Var(0)

val foo = fn {_: Unit ->
    val x = read(i)
    val label = Label("${x.deref()}")
    val button = Button(ButtonProps(text = "some button",
            onClick = {

            }))
    val button3 = button("foobar") {
        i.update { it + 1 }
        update()
    }
    val input = textField("${x.deref()}") {
        try {
            i.set(Integer.parseInt(it))
            update()
        } catch (e: NumberFormatException) {

        }
    }
    flowPanel {
        add(input)
        add(label)
        add(button)
        for (i in 0..x.deref()) {
            add(Label(i, "i is $i"))
        }
        add(button3)
    }
}

fun main(args: Array<String>) {
    noria({ f -> SwingUtilities.invokeLater(Runnable { f() }) }) {
        Frame(foo(Unit))
    }
}