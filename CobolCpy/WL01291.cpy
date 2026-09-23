000100 01  HEAD-WL01291.                                                        
000200*                                 COPYTEXT FOR DELIVERY NOTE LDC          
000300*                                 HEAD LINE                               
000400     03 HEAD-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 HEAD-REP-IDPTYP-1    PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 HEAD-REP-IDDC        PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 HEAD-REP-IDDISTR     PIC Z(3)9.                                   
001100*                                 DISTRIKTNUMMER                          
001200     03 HEAD-REP-IDKUNDNR    PIC Z(5)9.                                   
001300*                                 KUNDNUMMER                              
001400     03 HEAD-REP-IDORDNR     PIC Z(4)9.                                   
001500*                                 ORDERNUMMER UTGÅR PD90                  
001600     03 HEAD-REP-KDORDKL     PIC 9.                                       
001700*                                 ORDERKLASS                              
001800     03 HEAD-REP-IDBORD      PIC X(3).                                    
001900*                                 PACK-BORD                               
002000     03 HEAD-REP-IDUSER      PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200     03 HEAD-REP-IDPRODNR    PIC Z(6)9.                                   
002300*                                 PRODUKTIONSNUMMER                       
002400     03 HEAD-REP-BEGMT-RAD1  PIC X(35).                                   
002500*                                 GODSMOTTAGARNAMN RAD 1                  
002600     03 HEAD-REP-BEGMT-RAD2  PIC X(35).                                   
002700*                                 GODSMOTTAGARNAMN RAD 2                  
002800     03 HEAD-REP-ADGMT-GATA  PIC X(35).                                   
002900*                                 GODSMOTTAGARADRESS GATA                 
003000     03 HEAD-REP-ADGMT-PADR  PIC X(35).                                   
003100*                                 GODSMOTTAGARADRESS POSTADRESS           
003200     03 HEAD-REP-ADGMT-LAND  PIC X(35).                                   
003300*                                 GODSMOTTAGARADRESS LAND                 
003400     03 HEAD-REP-BEBETRAD-1  PIC X(35).                                   
003500     03 HEAD-REP-BETELNR     PIC X(20).                                   
003600     03 HEAD-REP-IDMAIL      PIC X(60).                                   
003700     03 HEAD-REP-IDLOPNR-ORD PIC Z(2)9.                                   
003800*                                 ORDERNS ORDNINGSNUMMER INOM             
003900*                                 EN PLOCKSATS                            
004000     03 HEAD-REP-BEKUNDRF    PIC X(15).                                   
004100*                                 KUNDENS REFERENS                        
004200     03 HEAD-REP-TIRFSDAT    PIC 9(6).                                    
004300*                                 KLART FÖR TRANSPORT ÅÅMMDD              
004400     03 HEAD-REP-TIRFSTID    PIC 9(4).                                    
004500*                                 KLART FÖR TRANSPORT (TTMM)              
004600     03 HEAD-REP-IDKOLLI-FOM PIC Z(4)9.                                   
004700*                                 KOLLINUMMER FRÅN OCH MED                
004800     03 HEAD-REP-IDKOLLI-TOM PIC Z(4)9.                                   
004900*                                 KOLLINUMMER TILL OCH MED                
005000     03 HEAD-REP-TIPACKN     PIC 9(6).                                    
005100*                                 PACKNINGSDATUM         (ÅÅMMDD)         
005200     03 HEAD-REP-TIPACTID    PIC 9(4).                                    
005300*                                 PACKNINGSTID  TTMMSS                    
005400*** END OF VILMAII-COPY LENGTH= 387 BYTES                                 
