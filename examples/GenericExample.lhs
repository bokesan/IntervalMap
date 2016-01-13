<h1>IntervalMap.generic usage example</h1>

In this example I use an IntervalMap to store a set of appointments.
The appointments are for several people, so they can overlap.

This is a literate Haskell file, you can [download it](Example.lhs) and compile it with
ghc or run in in ghci. You must first install IntervalMap, if you have not already done so:
`cabal install IntervalMap`.

First, I have to enable some language extensions to be able to
make my own interval type an instance of Interval:

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

In most cases, you should use the value-strict version, so I import that:

> import Data.IntervalMap.Generic.Strict

For readability, here are simple type synonyms for our data types. In production
code, one would of course use newtype or data for this.

> type Person = String
> type Details = String

Again for readability, I use strings to represent timestamps. Also, to keep it shorter,
I will omit the date and only use the time of the day in this example, e.g.: "09:00", "12:47".

> type Time = String
> type TimeSpan = (Time,Time)

I have a time span include its start time but not its end time.
So I declare an Interval instance for tuples that is closed at the startpoint
but open at the endpoint:

> instance Ord e => Interval (e,e) e where
>   lowerBound (a,_) = a
>   upperBound (_,b) = b
>   rightClosed _ = False

Constructing timespans is just using tuples:

> mkTimeSpan :: Time -> Time -> TimeSpan
> mkTimeSpan from to = (from,to)

An appointment consists of the timespan, the person, and the appointment details:

> type Appointment = (TimeSpan, Person, Details)

For a set of possible overlapping appointments, I store a list of (person, details)
tuples for each timespan:

> type Appointments = IntervalMap TimeSpan [(Person, Details)]

Now I can define some helper functions.

A set containing no appointments:

> noAppointments :: Appointments
> noAppointments =  empty

To add an appointment to a set, I use (++) as a combining function to add
the new appointment:

> addAppointment :: Person -> Time -> Time -> Details -> Appointments -> Appointments
> addAppointment who from to what = insertWith (++) (mkTimeSpan from to) [(who, what)]

To look up all appointments at a given time, just get all entries _containing_
that time:

> appointmentsAt :: Time -> Appointments -> [Appointment]
> appointmentsAt t apps = [ (time, person, details)
>                           | (time, pds) <- toAscList (apps `containing` t),
>                             (person, details) <- pds ]

The function to get all appointments that overlap a given timespan is almost
the same, just using _intersecting_ instead:

> appointmentsDuring :: Time -> Time -> Appointments -> [Appointment]
> appointmentsDuring from to apps =
>     [ (time, person, details)
>       | (time, pds) <- toAscList (apps `intersecting` mkTimeSpan from to),
>         (person, details) <- pds ]

Here is a sample set of appointments and a main function to show some test results:

> sampleApps :: Appointments
> sampleApps = addAppointment "Paul" "09:00" "11:00" "Dentist" $
>              addAppointment "John" "10:00" "11:30" "Meeting" $
>              addAppointment "Rosy" "10:00" "11:30" "Shopping" $
>              addAppointment "Lisa" "08:45" "09:15" "Bank" $
>              noAppointments
>
> main :: IO ()
> main = do putStrLn (show (appointmentsAt "09:00" sampleApps))
>           putStrLn (show (appointmentsDuring "09:30" "10:30" sampleApps))
