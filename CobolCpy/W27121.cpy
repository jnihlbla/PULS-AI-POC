000100 01  W27121.                                                              
000200*                                 AKTIVA ARTIKLAR PÅ WKD7                 
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 IDREFTAB             PIC X.                                       
001000*                                 IDENTITET REFILLTABELL                  
001100*                                 REFILLINGTABLE IDENTIFIER               
001200     03 FLREFBEO             PIC X.                                       
001300*                                 AUTOMATISK REFILL BEORDRING?            
001400*                                 AUTOMATIC REFILL ORDERING?              
001500     03 FLWILSON             PIC X.                                       
001600*                                 WILSONFORMEL                            
001700*                                 FLAG TO USE WILSON OR NOT               
001800     03 KDREFSTA             PIC X.                                       
001900*                                 STATUS REFILLARTIKEL                    
002000*                                 STATUS REFILLPART                       
002100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002200*                                 PRODUKTSLAG                             
002300*                                 PRODUCT GROUP                           
002400     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
002500*                                 PERIODBEHOV REFILLING                   
002600*                                 FORECAST REFILLING                      
002700     03 KVREFBER             PIC S9(7)           COMP-3.                  
002800*                                 BERÄKNAD REFILLINGKVANTITET             
002900*                                 CALCULATED REFILLING QUANTITY           
003000     03 KVREFOVL             PIC S9(7)           COMP-3.                  
003100*                                 BERÄKNAD ÖVERLAGERPUNKT                 
003200*                                 CALCULATED OVERSTOCK POINT              
003300     03 KVREFPKT             PIC S9(7)           COMP-3.                  
003400*                                 BERÄKNAD PÅFYLLNADSPUNKT                
003500*                                 CALCULATED REFILLING POINT              
003600     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003700*                                 ARTIKELSTANDARDPRIS                     
003800*                                 STANDARD PRICE                          
003900     03 TIREFPAF             PIC S9(7)           COMP-3.                  
004000*                                 DATUM MANUELL PÅFYLLNADSKVANT           
004100*                                 DATE MANUAL REFILLING QTY               
004200     03 TIREFPKT             PIC S9(7)           COMP-3.                  
004300*                                 DATUM MANUELL REFILLPUNKT               
004400*                                 DATE MANUAL REFILLING POINT             
004500     03 ADLAGOMR-DC          PIC S9(3)           COMP-3.                  
004600*                                 LAGEROMRÅDE                             
004700*                                 AREA                                    
004800     03 IDLEVNR-DC           PIC X(5).                                    
004900*                                 LEVERANTÖRNUMMER                        
005000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005100     03 RESEASON             OCCURS 12 TIMES                              
005200                             PIC S9V9(2)         COMP-3.                  
005300*                                 SÄSONGSINDEX                            
005400     03 KVPBREOI             PIC S9(6)V9(1)      COMP-3.                  
005500*                                 PERIODBEHOV FÖR REFILL OI               
005600*                                 PERIOD REQUIREM. REFILLING OI           
005700     03 IDDC-REF             PIC X(2).                                    
005800*                                 SÄNDANDE LAGER FÖR REFILL               
005900*                                 SENDING WAREHOUSE FOR REFILL            
006000*** END OF VILMAII-COPY LENGTH= 79 BYTES                                  
