In this example I use an IntervalMap to store a set of appointments.
The appointments are for several people, so it's clear that they can overlap.

This is a literate Haskell file, you can [download it](Example.lhs) and compile it with
ghc or run in in ghci. You must first install IntervalMap, if you have not already done so:
`cabal install IntervalMap`.

First I have to import the module:

> import Data.IntervalMap

For readability, here are simple type synonyms for our data types. In production
code, one would of course use newtype or data for this.

> type Person = String
> type Details = String

Again for readability, I use strings to represent timestamps. Also, to keep it shorter,
I will omit the date and only use the time of the day in this example, e.g.: "09:00", "12:47".

> type Time = String
> type TimeSpan = Interval Time

I have a time span include its start time but not its end time.
So I use the *IntervalCO* constructor to get an interval that is closed at the startpoint
but open at the endpoint:

> mkTimeSpan :: Time -> Time -> TimeSpan
> mkTimeSpan from to = IntervalCO from to

An appointment consists of the timespan, the person, and the appointment details:

> type Appointment = (TimeSpan, Person, Details)

For a set of possible overlapping appointments, I store a list of (person, details)
tuples for each timespan:

> type Appointments = IntervalMap Time [(Person, Details)]

Not that the key type is *Time*, not *TimeSpan*. That is, you specify the type of the
endpoints, not the type of the interval itself.

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
>                           | (time, pds) <- apps `containing` t,
>                             (person, details) <- pds ]

The function to get all appointments that overlap a given timespan is almost
the same, just using _intersecting_ instead:

> appointmentsDuring :: Time -> Time -> Appointments -> [Appointment]
> appointmentsDuring from to apps = [ (time, person, details)
>                                     | (time, pds) <- apps `intersecting` mkTimeSpan from to,
>                                       (person, details) <- pds ]

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
