000100 01  W23195-CTX.                                                          
000200*                                 COPYTEXT TILL FIL W23195                
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001000*                                 PRODUKTSLAG                             
001100     03 KDPSLLOC             PIC 9(2).                                    
001200*                                 PRODUKTSLAG LOKALT                      
001300     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001400*                                 PERSONKOD REFILLANSVARIG                
001500     03 KDFREKKL             PIC X.                                       
001600*                                 FREKVENSKLASS                           
001700     03 KDPRISKL             PIC X.                                       
001800*                                 PRISKLASS                               
001900     03 KDREFSTA             PIC X.                                       
002000*                                 STATUS REFILLARTIKEL                    
002100     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
002200*                                 DEL AV AK SOM LIGGER I SDC              
002300     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002400*                                 DEL AV AK PÅ VÄG                        
002500     03 KVLS                 PIC S9(7)           COMP-3.                  
002600*                                 LAGERSALDO                              
002700     03 KVOKS                PIC S9(7)           COMP-3.                  
002800*                                 ORDERKÖSALDO                            
002900     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
003000*                                 PERIODBEHOV REFILLING                   
003100     03 KVREFBER             PIC S9(7)           COMP-3.                  
003200*                                 BERÄKNAD REFILLINGKVANTITET             
003300     03 KVREFOVL             PIC S9(7)           COMP-3.                  
003400*                                 BERÄKNAD ÖVERLAGERPUNKT                 
003500     03 KVREFPKT             PIC S9(7)           COMP-3.                  
003600*                                 BERÄKNAD PÅFYLLNADSPUNKT                
003700     03 KVRESS               PIC S9(7)           COMP-3.                  
003800*                                 RESERVERAT ANTAL ARTIKLAR               
003900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 ARTIKELSTANDARDPRIS                     
004100     03 SUINKORD             PIC S9(18)          COMP-3.                  
004200*                                 SUMMA INKOMNA ORDERRADER PER            
004300*                                 BRYTBEGREPP                             
004400     03 SUFYSAVP             PIC S9(5)V9(2)      COMP-3.                  
004500*                                 SUMMA FYSISK AVVIKELSE PROCENT          
004600     03 SUAVBRP              PIC S9(5)V9(2)      COMP-3.                  
004700*                                 SUMMA AVBOKAD MÄNGD,DEL AV RAD          
004800     03 SULAGERB             PIC S9(18)          COMP-3.                  
004900*                                 SUMMA INKOMNA ORDERRADER PER            
005000*                                 BRYTBEGREPP                             
005100     03 SUSORTB              PIC S9(18)          COMP-3.                  
005200*                                 SUMMA INKOMNA ORDERRADER PER            
005300*                                 BRYTBEGREPP                             
005400     03 W23195-001-GRP       OCCURS 53 TIMES.                             
005500*                                 RULLANDE FÖRBRUKNING                    
005600        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
005700*                                 ORDERTRÄFFAR PÅ SDC                     
005800        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
005900*                                 ORDERTRÄFFAR LEV FRÅN CDC               
006000     03 W23195-002-GRP       OCCURS 5 TIMES.                              
006100*                                 INNEVARANDE FÖRBRUKNING                 
006200        05 TIVV              PIC S9(3)           COMP-3.                  
006300*                                 VECKA  (VV)                             
006400        05 KVOT-INNEV        PIC S9(7)           COMP-3.                  
006500*                                 ORDERTRÄFFAR PÅ SDC                     
006600        05 KVOT-CDC-INNEV    PIC S9(7)           COMP-3.                  
006700*                                 ORDERTRÄFFAR LEV FRÅN CDC               
006800     03 W23195-003-GRP       OCCURS 53 TIMES.                             
006900*                                 RULLANDE FÖRBRUKNING                    
007000        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
007100*                                 ORDERINGÅNG TILL SDC                    
007200     03 IDREFTAB             PIC X.                                       
007300*                                 IDENTITET REFILLTABELL                  
007400     03 PRMATRL              PIC S9(7)V9(2)      COMP-3.                  
007500*                                 FAST PRIS UNDER LÖPANDE ÅR              
007600*** END OF VILMAII-COPY LENGTH= 792 BYTES                                 
