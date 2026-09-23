000100 01  PRQU-W335PRQU.                                                       
000200*                                 LÄNKAREA TILL W335PRQU - UPPDAT         
000300*                                 ERA PRISFRÅGA                           
000400*                                 KDCALL = 1:           FÖRSTA FR         
000500*                                 ÅGENR FÖR ORDERN                        
000600*                                 KDCALL = 2:      EFTERFÖLJANDE          
000700*                                 RADER FÖR FRÅGAN                        
000800*                                 KDCALL = 3:        TA BORT FRÅG         
000900*                                 OR FÖR HEL ORDER                        
001000*                                 KDCALL = 4:       TA BORT FRÅGA         
001100*                                  FÖR ENSTAKA RAD                        
001200*                                 KDCALL = 5: ÄNDRA ANTAL I FRÅGA         
001300*                                  FÖR ENSTAKA RAD                        
001400     03 PRQU-KDCALL          PIC S9(3)           COMP-3.                  
001500*                                 ANROPSTYP                               
001600     03 PRQU-IDSYSTEM        PIC X(4).                                    
001700*                                 VOLVO VCCS SYSTEMNUMMER                 
001800     03 PRQU-IDDISTR         PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000     03 PRQU-IDKUNDNR        PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200     03 PRQU-IDKUNDRF-GRP.                                                
002300*                                 KUNDENS REFERENS (ORDERID)              
002400        05 PRQU-IDKUNDRF     PIC X(10).                                   
002500*                                 KUNDENS REFERENS (ORDERID)              
002600        05 PRQU-IDORDNR5-FILLER REDEFINES PRQU-IDKUNDRF.                  
002700           07 PRQU-IDORDNR5  PIC 9(5).                                    
002800*                                 ORDERNUMMER                             
002900           07 FILLER         PIC X(5).                                    
003000        05 PRQU-IDORDNR7-FILLER REDEFINES PRQU-IDKUNDRF.                  
003100           07 PRQU-IDORDNR7  PIC 9(7).                                    
003200*                                 ORDERNUMMER                             
003300           07 FILLER         PIC X(3).                                    
003400     03 PRQU-IDORDER         PIC S9(7)           COMP-3.                  
003500*                                 VOLVO PARTS ORDERNUMMER                 
003600     03 PRQU-KDORDKL         PIC S9              COMP-3.                  
003700*                                 ORDERKLASS                              
003800     03 PRQU-KDPRSTA         PIC X.                                       
003900*                                 STATUS PRISFRÅGA                        
004000     03 PRQU-IDARTNR         PIC S9(9)           COMP-3.                  
004100*                                 ARTIKELNUMMER                           
004200     03 PRQU-KVBEART-Q       PIC S9(7)           COMP-3.                  
004300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004400     03 PRQU-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
004500*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004600     03 PRQU-PRARTNTO-LOCPREL                                             
004700                             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 PREL NETTO SLUTKUNDSPRIS I              
004900*                                 LOKAL VALUTA                            
005000     03 PRQU-KDVALISO        PIC X(3).                                    
005100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005200     03 PRQU-IDPRQUES        PIC 9(7).                                    
005300*                                 PRISFRÅGA NR                            
005400     03 PRQU-FLPRTILL        PIC X.                                       
005500*                                 PRISTILLÄGGS FLAGGA                     
005600*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
