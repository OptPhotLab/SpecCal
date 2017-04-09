PRO Dialog_Choice_Event, ev

Widget_Control, ev.ID, Get_Value = thisChoice

Widget_Control, ev.Top, get_Uvalue = thisOne
*thisOne = thisChoice

Widget_Control, ev.Top, /destroy

END

FUNCTION Dialog_Choice, buttonNames, $
        MESSAGE_TEXT = message_text, $
        GROUP_LEADER = group_leader, $
        TITLE = title

If n_elements(title) EQ 0 then $
        title = 'Please make a selection'

if n_elements(message_text) EQ 0 then $
        message_text = 'Please make a selection'

if n_elements(buttonNames) EQ 0 then buttonNames = 'OK'

n_Choices = n_elements(buttonNames)

doModal =  ( n_elements(group_Leader) EQ 0 ) ? 0 : 1
base = widget_base(group_Leader = group_leader, $
        title = title, $
        Column = 1, $
        /Base_align_Center, xsize = 300, ysize = 70, xoffset = 400, yoffset = 300, /ALIGN_CENTER,TLB_FRAME_ATTR =11 $
        )

label = Widget_Label(base, $
        value = message_text, $
        /align_Center,font = 'Arial*14')

weeBase = Widget_Base(base, $
        Column = n_Choices, $
        /base_align_center)
For i = 0L, n_Choices-1 Do $
        button = WIDGET_BUTTON(weeBase, $
                Value = buttonNames[i], $
                Event_Pro = 'Dialog_Choice_Event', $
                uValue = buttonNames[i])
                
label2 = Widget_Label(base, $
        value = 'WARNING ! If "Append" is selected, old plot data may be overwritten !', $
        /align_Center, font =  'Arial*Italic*12')

ThisOne = Ptr_NEW("")
Widget_Control, base, Set_Uvalue = thisOne              
Widget_Control, base, /realize

XMANAGER, 'DIALOG_CHOICE', base

returnChoice = *thisone
Ptr_Free, thisOne
Return, returnChoice
END 