package graphs

/**
 * A network modified from that in http://snap.stanford.edu/data/egonets-Facebook.html
 * The anonymized properties have been renamed arbitrarily.
 */
object fbGraph {
  class Person(val id: Int, val isPhD: Boolean, val isFemale: Boolean,
    val speaksSpanish: Boolean, val speaksGerman: Boolean,
    val livesInEspoo: Boolean, val livesInHelsinki: Boolean,
    val company1: Boolean, val company2: Boolean, val company3: Boolean,
    val company4: Boolean, val company5: Boolean, val company6: Boolean) {
    override def toString: String = "p" + id
  }
  val persons = List(
    new Person(3981, false, false, false, false, false, true, false, false, false, true, false, false),
    new Person(3982, false, true, false, false, false, true, false, true, true, false, false, false),
    new Person(3983, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3984, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3985, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3986, true, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3987, false, false, false, false, true, false, false, false, false, false, false, false),
    new Person(3988, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3989, true, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3990, false, false, false, false, true, false, false, false, false, false, false, false),
    new Person(3991, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3992, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(3993, false, true, false, false, false, false, false, false, false, false, false, false),
    new Person(3994, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(3995, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(594, true, true, false, true, false, true, true, false, false, false, false, false),
    new Person(3996, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3997, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3998, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(3999, false, true, false, false, false, true, false, false, false, false, false, true),
    new Person(4000, true, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4001, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4002, false, true, false, true, false, true, true, false, false, false, false, false),
    new Person(4003, true, true, true, true, false, false, false, true, false, false, false, false),
    new Person(4004, true, true, false, true, false, true, false, false, false, false, true, false),
    new Person(4005, false, true, false, false, false, true, false, false, false, false, false, true),
    new Person(4006, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4007, false, true, false, false, true, false, false, false, false, false, false, false),
    new Person(4008, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4009, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4010, false, true, true, true, false, true, false, false, false, false, false, false),
    new Person(4011, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4012, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4013, false, false, false, false, false, true, false, false, false, true, false, false),
    new Person(4014, true, true, false, false, false, true, false, false, true, false, false, false),
    new Person(4015, false, true, false, false, true, false, false, false, false, false, false, false),
    new Person(4016, false, true, false, false, true, false, false, false, false, false, false, false),
    new Person(4017, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4018, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4019, true, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4020, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4021, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4022, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4023, false, true, false, false, false, true, false, false, false, false, true, false),
    new Person(4024, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4025, false, false, false, false, true, false, false, false, false, false, false, false),
    new Person(4026, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4027, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4028, false, true, false, true, false, true, true, false, false, false, false, false),
    new Person(4029, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4030, true, true, true, true, false, true, true, false, false, false, false, false),
    new Person(4031, false, true, false, false, false, true, true, false, false, false, true, false),
    new Person(4032, false, true, false, false, false, true, false, false, false, false, false, false),
    new Person(4033, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4034, true, true, false, true, false, true, false, false, false, false, true, false),
    new Person(4035, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4036, false, false, false, false, false, true, false, false, false, false, false, false),
    new Person(4037, false, true, false, true, false, false, false, false, false, false, false, false),
    new Person(4038, true, true, false, true, false, true, false, false, false, false, false, false))
  val connections = List((4001,4029),(4001,4036),(4010,4022),(4001,4029),(4035,4036),(4010,3985),(4012,4015),(4012,4027),(4025,4011),(4024, 4008),(3984,4008),(3984,3985),(4038, 4014), (4032, 4027), (4019, 4026), (4023, 4003), (4018, 3997), (4023, 4031), (4021, 3998), (4013, 4004), (4023, 4030), (4027, 4032), (3988, 4021), (3998, 3982), (4031, 4002), (4037, 4020), (3995, 3993), (4004, 3995), (3982, 3986), (4017, 3983), (3994, 3998), (3998, 3999), (4014, 3982), (3997, 3994), (4005, 3999), (3993, 3995), (4009, 3982), (4030, 3993), (3991, 3989), (3982, 4021), (3982, 4037), (4023, 4004), (3997, 4019), (3994, 4019), (4031, 4011), (4023, 3997), (4030, 4014), (3992, 4017), (3981, 3998), (3997, 4018), (4009, 4030), (3994, 4018), (3995, 4000), (4023, 4014), (4000, 4026), (4027, 4038), (4027, 4002), (4027, 4020), (4030, 4017), (4031, 4038), (4009, 3981), (4000, 4021), (3986, 4030), (3985, 4014), (3994, 4030), (3998, 4021), (3994, 4009), (3982, 4023), (4023, 4009), (3998, 4019), (4014, 3986), (4020, 4031), (4009, 4023), (3994, 3997), (3993, 3985), (4017, 3986), (4011, 594), (3995, 3985), (3981, 4023), (3995, 3988), (3997, 4030), (3997, 4021), (4030, 3997), (4000, 3992), (3996, 3994), (4038, 3989), (4029, 4001), (3989, 594), (4023, 4034), (3993, 4004), (4030, 4019), (4030, 3988), (3994, 3996), (4026, 4017), (4016, 3990), (4031, 3991), (4000, 4030), (3998, 4014), (4009, 3998), (4004, 4013), (4000, 3995), (3990, 4016), (3999, 4005), (4004, 4023), (4002, 4020), (3998, 4018), (4011, 3989), (4003, 3982), (4021, 3982), (4002, 3998), (4031, 4020), (3985, 3995), (3988, 3985), (3989, 3991), (4000, 4017), (4003, 4009), (3997, 3981), (3982, 4030), (3982, 3994), (3998, 4005), (3995, 4014), (4021, 4030), (4005, 3998), (4023, 3998), (594, 4011), (3993, 4030), (4020, 4030), (3989, 4038), (3989, 4011), (4019, 3994), (4030, 4020), (4009, 4019), (4004, 4020), (3995, 4026), (4023, 3981), (4020, 4002), (4014, 3985), (4017, 4026), (3989, 4013), (4038, 4023), (4023, 4002), (4020, 4037), (3998, 4002), (4013, 3989), (3995, 4023), (4021, 3988), (3983, 4017), (4004, 3993), (4021, 4000), (3999, 4036), (4026, 4019), (4033, 3986), (4023, 3995), (3982, 3997), (3998, 3981), (3990, 4007), (3985, 3988), (4018, 3981), (4018, 4030), (4025, 3990), (4026, 4030), (4021, 3997), (3997, 4023), (4031, 594), (3996, 4028), (3982, 3988), (4012, 3987), (4021, 4017), (3988, 4030), (4013, 4023), (4025, 4007), (4014, 4021), (4030, 4004), (4014, 4037), (3986, 4021), (4017, 4021), (3982, 4009), (4016, 4007), (3998, 4023), (3998, 4009), (4007, 3990), (594, 3989), (4009, 3997), (3992, 4000), (4011, 4031), (3986, 3982), (4019, 4030), (4020, 4038), (4020, 4004), (4021, 4014), (4023, 3982), (4017, 4000), (3997, 3998), (3993, 3988), (4030, 3994), (4023, 4038), (4004, 4031), (4027, 4031), (4014, 4038), (3986, 4000), (4019, 3997), (3982, 4003), (3986, 4033), (4030, 4003), (4030, 4018), (3981, 3994), (4004, 4038), (4009, 4003), (3985, 3993), (4000, 4033), (4013, 4038), (4018, 4023), (4036, 3999), (4003, 4030), (4019, 4009), (4030, 4023), (4026, 4021), (4019, 3981), (3990, 4025), (4023, 4018), (4021, 3986), (4030, 4009), (3986, 4026), (4038, 4020), (4014, 3995), (3996, 4002), (4031, 4004), (4001, 4029), (4038, 4013), (4014, 4030), (4020, 4027), (3988, 3982), (3998, 3994), (3982, 3998), (4033, 4000), (3988, 3993), (4002, 4031), (3998, 3997), (3988, 3995), (4037, 3982), (3994, 3982), (4018, 3994), (3986, 4014), (4003, 4023), (4037, 4014), (4026, 3995), (3981, 4019), (3999, 3998), (3997, 4009), (4026, 4000), (4014, 4023), (4004, 4030), (4038, 4027), (4006, 4027), (594, 4031), (4007, 4025), (4038, 4004), (3981, 4018), (3981, 3997), (4023, 4013), (3982, 4026), (4014, 4017), (4009, 3994), (3991, 4031), (4000, 3986), (3994, 3981), (4018, 3998), (3987, 4012), (4019, 3998), (4030, 4026), (4007, 4016), (4030, 4000), (4017, 4014), (3995, 4004), (3997, 3982), (4014, 3998), (4017, 4030), (4002, 4023), (3994, 4023), (4004, 3985), (4026, 3982), (4034, 4023), (4030, 3982), (4023, 3994), (4002, 3996), (3982, 4014), (3981, 4009), (4021, 4026), (4030, 3986), (4013, 4031), (4026, 3986), (4027, 4006), (4030, 4021), (3986, 4017), (4017, 3992), (4038, 4031), (4031, 4023), (4031, 4027), (4002, 4027), (4031, 4013), (4028, 3996), (3985, 4004))
  lazy val idToPerson: Map[Int, Person] = persons.map(p => (p.id, p)).toMap
  def getGraph: (Graph, Map[String, Int]) = {
    val personToIndex = persons.zipWithIndex.toMap
    val nofVertices = persons.size
    val edges = connections.map({case(id1, id2) => (personToIndex(idToPerson(id1)), personToIndex(idToPerson(id2)))})
    val nameToIndex = persons.zipWithIndex.map({case(p, i) => (p.toString, i)}).toMap
    (new Graph(nofVertices, edges), nameToIndex)
  }
}
