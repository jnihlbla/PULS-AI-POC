000100 01  SLAG-W27147.                                                         
000200*                                 UTDRAG UR WDK701 OCH WDK711             
000300*                                                                         
000400     03 SLAG-IDARTNR         PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 SLAG-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 SLAG-ADART.                                                       
001100*                                 ARTIKELADRESS I LAGRET                  
001200*                                 PARTS-ADRESS                            
001300        05 SLAG-ADLAGOMR     PIC S9(3)           COMP-3.                  
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600        05 SLAG-ADGANG       PIC S9(3)           COMP-3.                  
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900        05 SLAG-ADPLATS      PIC S9(5)           COMP-3.                  
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200     03 SLAG-DASPSEA         PIC 9(8).                                    
002300*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
002400*                                 DATE SEASON BLOCKED TO YYYYMMDD         
002500     03 SLAG-FLREFBEO        PIC X.                                       
002600*                                 AUTOMATISK REFILL BEORDRING?            
002700*                                 AUTOMATIC REFILL ORDERING?              
002800     03 SLAG-FLWILSON        PIC X.                                       
002900*                                 WILSONFORMEL                            
003000*                                 FLAG TO USE WILSON OR NOT               
003100     03 SLAG-IDREFTAB        PIC X.                                       
003200*                                 IDENTITET REFILLTABELL                  
003300*                                 REFILLINGTABLE IDENTIFIER               
003400     03 SLAG-IDLEVNR         PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003700     03 SLAG-KDREFSTA        PIC X.                                       
003800*                                 STATUS REFILLARTIKEL                    
003900*                                 STATUS REFILLPART                       
004000     03 SLAG-KVPB-REF        PIC S9(6)V9(1)      COMP-3.                  
004100*                                 PERIODBEHOV REFILLING                   
004200*                                 FORECAST REFILLING                      
004300     03 SLAG-KVREFBER        PIC S9(7)           COMP-3.                  
004400*                                 BERÄKNAD REFILLINGKVANTITET             
004500*                                 CALCULATED REFILLING QUANTITY           
004600     03 SLAG-KVREFOVL        PIC S9(7)           COMP-3.                  
004700*                                 BERÄKNAD ÖVERLAGERPUNKT                 
004800*                                 CALCULATED OVERSTOCK POINT              
004900     03 SLAG-KVREFPKT        PIC S9(7)           COMP-3.                  
005000*                                 BERÄKNAD PÅFYLLNADSPUNKT                
005100*                                 CALCULATED REFILLING POINT              
005200     03 SLAG-RESEASON        OCCURS 12 TIMES                              
005300                             PIC S9V9(2)         COMP-3.                  
005400*                                 SÄSONGSINDEX                            
005500     03 SLAG-TIREFPAF        PIC S9(7)           COMP-3.                  
005600*                                 DATUM MANUELL PÅFYLLNADSKVANT           
005700*                                 DATE MANUAL REFILLING QTY               
005800     03 SLAG-TIREFPKT        PIC S9(7)           COMP-3.                  
005900*                                 DATUM MANUELL REFILLPUNKT               
006000*                                 DATE MANUAL REFILLING POINT             
006100     03 SLAG-KVPBREOI        PIC S9(6)V9(1)      COMP-3.                  
006200*                                 PERIODBEHOV FÖR REFILL OI               
006300*                                 PERIOD REQUIREM. REFILLING OI           
006400     03 SLAG-IDDC-REF        PIC X(2).                                    
006500*                                 SÄNDANDE LAGER FÖR REFILL               
006600*                                 SENDING WAREHOUSE FOR REFILL            
006700*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
