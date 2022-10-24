{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Event-Instanced where

open import Ffi.Hs.SDL.Event

instance
    inst:Bounded[InputMotion] = Bounded[InputMotion]
    inst:Enum[InputMotion]    = Enum[InputMotion]
    inst:Eq[InputMotion]      = Eq[InputMotion]
    inst:Data[InputMotion]    = Data[InputMotion]
    inst:Ord[InputMotion]     = Ord[InputMotion]
    inst:Read[InputMotion]    = Read[InputMotion]
    inst:Show[InputMotion]    = Show[InputMotion]

    inst:Eq[WindowShownEventData]   = Eq[WindowShownEventData]
    inst:Ord[WindowShownEventData]  = Ord[WindowShownEventData]
    inst:Show[WindowShownEventData] = Show[WindowShownEventData]

    inst:Eq[WindowHiddenEventData]   = Eq[WindowHiddenEventData]
    inst:Ord[WindowHiddenEventData]  = Ord[WindowHiddenEventData]
    inst:Show[WindowHiddenEventData] = Show[WindowHiddenEventData]

    inst:Eq[WindowExposedEventData]   = Eq[WindowExposedEventData]
    inst:Ord[WindowExposedEventData]  = Ord[WindowExposedEventData]
    inst:Show[WindowExposedEventData] = Show[WindowExposedEventData]

    inst:Eq[WindowMovedEventData]   = Eq[WindowMovedEventData]
    inst:Ord[WindowMovedEventData]  = Ord[WindowMovedEventData]
    inst:Show[WindowMovedEventData] = Show[WindowMovedEventData]

    inst:Eq[WindowResizedEventData]   = Eq[WindowResizedEventData]
    inst:Ord[WindowResizedEventData]  = Ord[WindowResizedEventData]
    inst:Show[WindowResizedEventData] = Show[WindowResizedEventData]

    inst:Eq[WindowSizeChangedEventData]   = Eq[WindowSizeChangedEventData]
    inst:Ord[WindowSizeChangedEventData]  = Ord[WindowSizeChangedEventData]
    inst:Show[WindowSizeChangedEventData] = Show[WindowSizeChangedEventData]

    inst:Eq[WindowMinimizedEventData]   = Eq[WindowMinimizedEventData]
    inst:Ord[WindowMinimizedEventData]  = Ord[WindowMinimizedEventData]
    inst:Show[WindowMinimizedEventData] = Show[WindowMinimizedEventData]

    inst:Eq[WindowMaximizedEventData]   = Eq[WindowMaximizedEventData]
    inst:Ord[WindowMaximizedEventData]  = Ord[WindowMaximizedEventData]
    inst:Show[WindowMaximizedEventData] = Show[WindowMaximizedEventData]

    inst:Eq[WindowRestoredEventData]   = Eq[WindowRestoredEventData]
    inst:Ord[WindowRestoredEventData]  = Ord[WindowRestoredEventData]
    inst:Show[WindowRestoredEventData] = Show[WindowRestoredEventData]

    inst:Eq[WindowGainedMouseFocusEventData]   = Eq[WindowGainedMouseFocusEventData]
    inst:Ord[WindowGainedMouseFocusEventData]  = Ord[WindowGainedMouseFocusEventData]
    inst:Show[WindowGainedMouseFocusEventData] = Show[WindowGainedMouseFocusEventData]

    inst:Eq[WindowLostMouseFocusEventData]   = Eq[WindowLostMouseFocusEventData]
    inst:Ord[WindowLostMouseFocusEventData]  = Ord[WindowLostMouseFocusEventData]
    inst:Show[WindowLostMouseFocusEventData] = Show[WindowLostMouseFocusEventData]

    inst:Eq[WindowGainedKeyboardFocusEventData]   = Eq[WindowGainedKeyboardFocusEventData]
    inst:Ord[WindowGainedKeyboardFocusEventData]  = Ord[WindowGainedKeyboardFocusEventData]
    inst:Show[WindowGainedKeyboardFocusEventData] = Show[WindowGainedKeyboardFocusEventData]

    inst:Eq[WindowLostKeyboardFocusEventData]   = Eq[WindowLostKeyboardFocusEventData]
    inst:Ord[WindowLostKeyboardFocusEventData]  = Ord[WindowLostKeyboardFocusEventData]
    inst:Show[WindowLostKeyboardFocusEventData] = Show[WindowLostKeyboardFocusEventData]

    inst:Eq[WindowClosedEventData]   = Eq[WindowClosedEventData]
    inst:Ord[WindowClosedEventData]  = Ord[WindowClosedEventData]
    inst:Show[WindowClosedEventData] = Show[WindowClosedEventData]

    inst:Eq[SysWMEventData]   = Eq[SysWMEventData]
    inst:Ord[SysWMEventData]  = Ord[SysWMEventData]
    inst:Show[SysWMEventData] = Show[SysWMEventData]

    inst:Eq[KeyboardEventData]   = Eq[KeyboardEventData]
    inst:Ord[KeyboardEventData]  = Ord[KeyboardEventData]
    inst:Show[KeyboardEventData] = Show[KeyboardEventData]

    inst:Eq[TextEditingEventData]   = Eq[TextEditingEventData]
    inst:Ord[TextEditingEventData]  = Ord[TextEditingEventData]
    inst:Show[TextEditingEventData] = Show[TextEditingEventData]

    inst:Eq[TextInputEventData]   = Eq[TextInputEventData]
    inst:Ord[TextInputEventData]  = Ord[TextInputEventData]
    inst:Show[TextInputEventData] = Show[TextInputEventData]

    inst:Eq[MouseMotionEventData]   = Eq[MouseMotionEventData]
    inst:Ord[MouseMotionEventData]  = Ord[MouseMotionEventData]
    inst:Show[MouseMotionEventData] = Show[MouseMotionEventData]

    inst:Eq[MouseButtonEventData]   = Eq[MouseButtonEventData]
    inst:Ord[MouseButtonEventData]  = Ord[MouseButtonEventData]
    inst:Show[MouseButtonEventData] = Show[MouseButtonEventData]

    inst:Eq[MouseWheelEventData]   = Eq[MouseWheelEventData]
    inst:Ord[MouseWheelEventData]  = Ord[MouseWheelEventData]
    inst:Show[MouseWheelEventData] = Show[MouseWheelEventData]

    inst:Eq[JoyAxisEventData]   = Eq[JoyAxisEventData]
    inst:Ord[JoyAxisEventData]  = Ord[JoyAxisEventData]
    inst:Show[JoyAxisEventData] = Show[JoyAxisEventData]

    inst:Eq[JoyBallEventData]   = Eq[JoyBallEventData]
    inst:Ord[JoyBallEventData]  = Ord[JoyBallEventData]
    inst:Show[JoyBallEventData] = Show[JoyBallEventData]

    inst:Eq[JoyHatEventData]   = Eq[JoyHatEventData]
    inst:Ord[JoyHatEventData]  = Ord[JoyHatEventData]
    inst:Show[JoyHatEventData] = Show[JoyHatEventData]

    inst:Eq[JoyButtonEventData]   = Eq[JoyButtonEventData]
    inst:Ord[JoyButtonEventData]  = Ord[JoyButtonEventData]
    inst:Show[JoyButtonEventData] = Show[JoyButtonEventData]

    inst:Eq[JoyDeviceEventData]   = Eq[JoyDeviceEventData]
    inst:Ord[JoyDeviceEventData]  = Ord[JoyDeviceEventData]
    inst:Show[JoyDeviceEventData] = Show[JoyDeviceEventData]

    inst:Eq[ControllerAxisEventData]   = Eq[ControllerAxisEventData]
    inst:Ord[ControllerAxisEventData]  = Ord[ControllerAxisEventData]
    inst:Show[ControllerAxisEventData] = Show[ControllerAxisEventData]

    inst:Eq[ControllerButtonEventData]   = Eq[ControllerButtonEventData]
    inst:Ord[ControllerButtonEventData]  = Ord[ControllerButtonEventData]
    inst:Show[ControllerButtonEventData] = Show[ControllerButtonEventData]

    inst:Eq[ControllerDeviceEventData]   = Eq[ControllerDeviceEventData]
    inst:Ord[ControllerDeviceEventData]  = Ord[ControllerDeviceEventData]
    inst:Show[ControllerDeviceEventData] = Show[ControllerDeviceEventData]

    inst:Eq[AudioDeviceEventData]   = Eq[AudioDeviceEventData]
    inst:Ord[AudioDeviceEventData]  = Ord[AudioDeviceEventData]
    inst:Show[AudioDeviceEventData] = Show[AudioDeviceEventData]

    inst:Eq[UserEventData]   = Eq[UserEventData]
    inst:Ord[UserEventData]  = Ord[UserEventData]
    inst:Show[UserEventData] = Show[UserEventData]

    inst:Eq[TouchFingerEventData]   = Eq[TouchFingerEventData]
    inst:Ord[TouchFingerEventData]  = Ord[TouchFingerEventData]
    inst:Show[TouchFingerEventData] = Show[TouchFingerEventData]

    inst:Eq[TouchFingerMotionEventData]   = Eq[TouchFingerMotionEventData]
    inst:Ord[TouchFingerMotionEventData]  = Ord[TouchFingerMotionEventData]
    inst:Show[TouchFingerMotionEventData] = Show[TouchFingerMotionEventData]

    inst:Eq[MultiGestureEventData]   = Eq[MultiGestureEventData]
    inst:Ord[MultiGestureEventData]  = Ord[MultiGestureEventData]
    inst:Show[MultiGestureEventData] = Show[MultiGestureEventData]

    inst:Eq[DollarGestureEventData]   = Eq[DollarGestureEventData]
    inst:Ord[DollarGestureEventData]  = Ord[DollarGestureEventData]
    inst:Show[DollarGestureEventData] = Show[DollarGestureEventData]

    inst:Eq[DropEventData]   = Eq[DropEventData]
    inst:Ord[DropEventData]  = Ord[DropEventData]
    inst:Show[DropEventData] = Show[DropEventData]

    inst:Eq[UnknownEventData]   = Eq[UnknownEventData]
    inst:Ord[UnknownEventData]  = Ord[UnknownEventData]
    inst:Show[UnknownEventData] = Show[UnknownEventData]

    inst:Eq[EventPayload]   = Eq[EventPayload]
    inst:Ord[EventPayload]  = Ord[EventPayload]
    inst:Show[EventPayload] = Show[EventPayload]

    inst:Eq[Event]   = Eq[Event]
    inst:Ord[Event]  = Ord[Event]
    inst:Show[Event] = Show[Event]

    inst:Eq[EventPushResult]   = Eq[EventPushResult]
    inst:Data[EventPushResult] = Data[EventPushResult]
    inst:Ord[EventPushResult]  = Ord[EventPushResult]
    inst:Read[EventPushResult] = Read[EventPushResult]
    inst:Show[EventPushResult] = Show[EventPushResult]

    inst:Eq[RegisteredEventData]   = Eq[RegisteredEventData]
    inst:Ord[RegisteredEventData]  = Ord[RegisteredEventData]
    inst:Show[RegisteredEventData] = Show[RegisteredEventData]
