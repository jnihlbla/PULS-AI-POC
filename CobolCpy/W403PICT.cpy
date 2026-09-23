000100 01  PICT-W403PICT.                                                       
000200*                                 3IV PICK TASK POST                      
000300*                                 3IV PICK TASK RECORD                    
000400*                                 IDRTYP3IV="PickTask"                    
000500     03 PICT-IDRTYP3IV       PIC X(30).                                   
000600*                                 3IV RECORD-TYP                          
000700*                                 3IV RECORD TYPE                         
000800     03 PICT-IDPURAD         PIC Z(3)9.                                   
000900*                                 RADNUMMER PÅ PACKUNDERLAG               
001000*                                 LINENO IN PACKINGDOCUMENT               
001100     03 PICT-IDLOPNR-ORD     PIC Z(2)9.                                   
001200*                                 ORDERNS ORDNINGSNUMMER INOM             
001300*                                 EN PLOCKSATS                            
001400*                                 SEQUENCE-NUMBER FOR AN ORDER            
001500*                                 WITHIN A PICKING UNIT                   
001600     03 PICT-ADLOC3IV        PIC X(10).                                   
001700*                                 LAGERPLATS (OOGG PPPPP)                 
001800*                                 STORAGE ADDRESS (AABB PPPPP)            
001900     03 PICT-ADPLATS-5       PIC X.                                       
002000     03 PICT-ADPLATS-4       PIC X.                                       
002100     03 PICT-ADPLATS-1-3     PIC X(3).                                    
002200     03 PICT-ADGANG          PIC 9(2).                                    
002300*                                 GÅNG                                    
002400*                                 AISLE                                   
002500     03 PICT-ADLAGOMR        PIC 9(2).                                    
002600*                                 LAGEROMRÅDE                             
002700*                                 AREA                                    
002800     03 PICT-KDLCD3IV        PIC 9(3).                                    
002900*                                 KONTROLLSIFFOR FÖR PLATS                
003000*                                 CHECK DIGITS FOR LOCATION               
003100     03 PICT-KVAVBART        PIC Z(5)9.                                   
003200*                                 AVBOKAT ANTAL ARTIKLAR                  
003300*                                 ALLOCATED QUANTITY                      
003400     03 PICT-KDSORT          PIC X(2).                                    
003500*                                 SORT-KOD                                
003600*                                 UNIT OF MEASURE                         
003700     03 PICT-TESORT          PIC X(30).                                   
003800*                                 ORD FÖR SORT/ENHET                      
003900*                                 NAME OF UNIT OF MEASURE                 
004000     03 PICT-IDARTNR         PIC Z(8)9.                                   
004100*                                 ARTIKELNUMMER                           
004200*                                 PART NUMBER                             
004300     03 PICT-BEART           PIC X(25).                                   
004400*                                 ARTIKELBENÄMNING                        
004500*                                 PART DESCRIPTION                        
004600     03 PICT-KDARTURS-NUM    PIC X(2).                                    
004700*                                 ARTIKELURSPRUNGSKOD NUMERISK            
004800*                                 COUNTRY OF ORIGIN NUMERIC               
004900     03 PICT-BEARTURS-SVE    PIC X(15).                                   
005000*                                 ARTIKELNS URPRUNGSLAND                  
005100*                                 I KLARTEXT SVENSKA                      
005200*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
