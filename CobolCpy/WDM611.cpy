000100 01  OBJ-WDM611.                                                          
000200*                                 BYTESREGISTER                           
000300*                                 LAGRING AV RETURER                      
000400*                                 OBJEKT SEGMENT                          
000500*                                 FYSISK NYCKEL: IDBYTRAD                 
000600     03 OBJ-IDBYTRAD         PIC S9(5)           COMP-3.                  
000700*                                 RADNUMMER                               
000800*                                 LINE NO                                 
000900     03 OBJ-IDARTNR-OBJ      PIC S9(9)           COMP-3.                  
001000*                                 OBJEKTNUMMER                            
001100     03 OBJ-IDTABNR          PIC S9(3)           COMP-3.                  
001200*                                 TABELLNUMMER                            
001300*                                 TABELNUMBER                             
001400     03 OBJ-BERADREF         PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600*                                 CUSTOMERS ITEM REF.                     
001700     03 OBJ-IDORDER          PIC S9(7)           COMP-3.                  
001800*                                 VOLVO PARTS ORDERNUMMER                 
001900*                                 VOLVO PARTS ORDER NUMBER                
002000     03 OBJ-KDBYTREF         PIC X(3).                                    
002100*                                 CENTRAL REFERENS                        
002200*                                 CENTRAL REFERENCE                       
002300     03 OBJ-KDBYTSTA-AVL     PIC X.                                       
002400*                                 AVLÄST STATUSKOD BYTESOBJEKT            
002500*                                 READ STATUS CODE EXCHANGE CORES         
002600     03 OBJ-KDBYTSTA-OBJ     PIC X.                                       
002700*                                 STATUSKOD BYTESOBJEKT                   
002800*                                 STATUSCODE EXCH CORES                   
002900     03 OBJ-KVRETUR-GODK     PIC S9(7)           COMP-3.                  
003000*                                 ANTAL GODKÄNDA BYTESOBJEKT              
003100*                                 QUANTITY APPROVED OBJECT                
003200     03 OBJ-KVRETUR-URSP     PIC S9(7)           COMP-3.                  
003300*                                 ANTAL OBJEKT RETURER.                   
003400*                                 QUANTITY OBJECT RETURNS                 
003500     03 OBJ-IDBYTRAP-9KOMPL  PIC S9(7)           COMP-3.                  
003600*                                 RAPPORTNUMMER BYTES 9-KOMPL             
003700*                                 REPORTNUMBER  EXCHANGE 9-COMPL          
003800     03 OBJ-FLSKROT          PIC X.                                       
003900*                                 SKROTNINGSMARKERING                     
004000*                                 SCRAPPING FLAG                          
004100     03 OBJ-FILLER           PIC X(10).                                   
004200*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
