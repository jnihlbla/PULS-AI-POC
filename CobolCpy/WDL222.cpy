000100 01  DIR-WDL222.                                                          
000200*                                 INLEVERANS HISTORIK                     
000300*                                 R33 DIREKT LEVERANS                     
000400*                                 R34 DIREKT INLÄGGNING                   
000500*                                 FYSISK NYCKEL SAKNAS                    
000600*                                 SÖKBEGREPP IDPTYP                       
000700*                                            IDLOPNRM                     
000800     03 DIR-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 DIR-IDLOPNRM         PIC S9(9)           COMP-3.                  
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300     03 DIR-IDAVINR          PIC S9(7)           COMP-3.                  
001400*                                 AVI-NUMMER                              
001500     03 DIR-IDANALYS         PIC X(12).                                   
001600*                                 ANALYSNUMMER                            
001700     03 DIR-IDDC             PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 DIR-IDKONTO          PIC S9(11)          COMP-3.                  
002000*                                 KONTO                                   
002100     03 DIR-IDKST            PIC X(10).                                   
002200*                                 KOSTNADSSTÄLLE                          
002300     03 DIR-IDLEVNR          PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500     03 DIR-KDRT             PIC S9(3)           COMP-3.                  
002600*                                 REDOVISNINGSTYP                         
002700     03 DIR-KVAVIS           PIC S9(7)           COMP-3.                  
002800*                                 AVISERAT ANTAL                          
002900     03 DIR-TIAVSDAT         PIC S9(7)           COMP-3.                  
003000*                                 AVISERINGSDATUM (YYMMDD)                
003100     03 DIR-IDGMTREF.                                                     
003200*                                 GODSMOTTAGAREREFERENS                   
003300        05 DIR-IDDISTR       PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500        05 DIR-IDKUNDNR      PIC S9(7)           COMP-3.                  
003600*                                 KUNDNUMMER                              
003700        05 DIR-IDKUNDRF-GRP.                                              
003800*                                 KUNDENS REFERENS (ORDERID)              
003900           07 DIR-IDKUNDRF   PIC X(10).                                   
004000*                                 KUNDENS REFERENS (ORDERID)              
004100           07 DIR-IDORDNR5-FILLER REDEFINES DIR-IDKUNDRF.                 
004200              09 DIR-IDORDNR5                                             
004300                             PIC 9(5).                                    
004400*                                 ORDERNUMMER                             
004500              09 FILLER      PIC X(5).                                    
004600           07 DIR-IDORDNR7-FILLER REDEFINES DIR-IDKUNDRF.                 
004700              09 DIR-IDORDNR7                                             
004800                             PIC 9(7).                                    
004900*                                 ORDERNUMMER                             
005000              09 FILLER      PIC X(3).                                    
005100     03 DIR-IDPRODNR         PIC S9(7)           COMP-3.                  
005200*                                 PRODUKTIONSNUMMER                       
005300     03 DIR-IDFAKT           PIC S9(7)           COMP-3.                  
005400*                                 FAKTURANUMMER                           
005500     03 DIR-IDSUPREF         PIC X(10).                                   
005600*                                 LEVERANTöRSREF.                         
005700*** END OF VILMAII-COPY LENGTH= 92 BYTES                                  
