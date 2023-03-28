package u04lab.code

import List.*

trait Item {
  def code: Int

  def name: String

  def tags: List[String]
}

object Item:
  import Stream.*
  def apply(code: Int, name: String, tags: List[String]): Item = ItemImpl(code, name, tags)

  //def apply(code: Int, name: String, tags: String*): Item = ItemImpl(code, name, tags.collect{ case mystr: String => mystr })

  private case class ItemImpl(override val code: Int, override val name: String, override val tags: List[String]) extends Item

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse {
  /**
   * Stores an item in the warehouse.
   *
   * @param item the item to store
   */
  def store(item: Item): Unit

  /**
   * Searches for items with the given tag.
   *
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): List[Item]

  /**
   * Retrieves an item from the warehouse.
   *
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Option[Item]

  /**
   * Removes an item from the warehouse.
   *
   * @param item the item to remove
   */
  def remove(item: Item): Unit

  /**
   * Checks if the warehouse contains an item with the given code.
   *
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
}

import u04lab.code.List.*

object Warehouse {

  def apply(): Warehouse = WarehouseImpl()

  private case class WarehouseImpl() extends Warehouse :
    private var items: List[Item] = List.empty

    override def store(item: Item): Unit =
      items = append(items, Cons(item, Nil()))

    override def contains(itemCode: Int): Boolean =
      List.contains(map(items)(item => item.code), itemCode)

    override def searchItems(tag: String): List[Item] =
      filter(items)(item => List.contains(item.tags, tag))


    override def retrieve(code: Int): Option[Item] =
      find(items)(item => item.code == code)

    override def remove(item: Item): Unit =
      items = filter(items)(myItem => myItem != item)


}

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", cons("notebook", empty))
  val dellInspiron = Item(34, "Dell Inspiron 13", cons("notebook", empty))
  val xiaomiMoped = Item(35, "Xiaomi S1", cons("moped", cons("mobility", empty)))

  println(warehouse.contains(dellXps.code)) // false
  warehouse.store(dellXps)
  println(warehouse.contains(dellXps.code)) // true
  warehouse.store(dellInspiron)
  warehouse.store(xiaomiMoped)
  println(warehouse.searchItems("mobility")) // List(xiaomiMoped)
  println(warehouse.searchItems("notebook")) // List(dellXps, dellInspiron)
  println(warehouse.retrieve(11)) // None
  println(warehouse.retrieve(dellXps.code)) // Some(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  println(warehouse.retrieve(dellXps.code)) // None

  /**
  * - Refactor the code of Item accepting a variable number of tags (hint: use _*) */