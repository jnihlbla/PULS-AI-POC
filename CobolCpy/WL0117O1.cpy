000100 01  RESP-WL0117O1.                                                       
000200*                                 RESPONS FROM PGM WL0117                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDORDNR-KEY     PIC Z(5).                                    
001000*                                 ORDERNUMMER                             
001100     03 RESP-IDPRODNR-KEY    PIC Z(7).                                    
001200*                                 PRODUKTIONSNUMMER                       
001300     03 RESP-IDDISTR         PIC Z(3)9.                                   
001400*                                 DISTRIKTNUMMER                          
001500     03 RESP-IDKUNDNR        PIC Z(5)9.                                   
001600*                                 KUNDNUMMER                              
001700     03 RESP-KDFRAKT         PIC Z9.                                      
001800*                                 FRAKTSÄTT DC TILL KUND                  
001900     03 RESP-IDKUNDRF        PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100     03 RESP-BEGMT-RAD1      PIC X(35).                                   
002200*                                 GODSMOTTAGARNAMN RAD 1                  
002300     03 RESP-UTSKRIFTSDATUM  PIC X(13).                                   
002400     03 RESP-BEGMT-RAD2      PIC X(35).                                   
002500*                                 GODSMOTTAGARNAMN RAD 2                  
002600     03 RESP-ADGMT-GATA      PIC X(35).                                   
002700*                                 GODSMOTTAGARADRESS GATA                 
002800     03 RESP-BEVARREF        PIC X(10).                                   
002900*                                 VÅR REFERENS                            
003000     03 RESP-TIORDREG        PIC 9(6).                                    
003100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003200     03 RESP-KDORDKL         PIC 9.                                       
003300*                                 ORDERKLASS                              
003400     03 RESP-TIBEGPAC        PIC 9(6).                                    
003500*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
003600     03 RESP-ADGMT-PADR      PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS POSTADRESS           
003800     03 RESP-TIPACKN-SK      PIC 9(6).                                    
003900*                                 PACKNINGSDATUM SENASTE KOLLI            
004000     03 RESP-ADGMT-LAND      PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS LAND                 
004200     03 RESP-BEFRAKT         PIC X(20).                                   
004300*                                 FRAKT TEXT                              
004400     03 RESP-VAGNNR          PIC X(8).                                    
004500     03 RESP-IDUSER          PIC X(8).                                    
004600*                                 ANVÄNDARENS SÄKERHETS ID                
004700     03 RESP-BETELNR         PIC X(10).                                   
004800*                                 TELEFONNUMMER                           
004900     03 RESP-VLORDNTO        PIC Z(3)9.9(3).                              
005000*                                 ORDERVOLYM NETTO (M3)                   
005100     03 RESP-VKORDNTO        PIC Z(5)9.9.                                 
005200*                                 ORDERVIKT NETTO (KG)                    
005300     03 RESP-KVORDRAD        PIC Z(4)9.                                   
005400*                                 ANTAL ORDERRADER                        
005500     03 RESP-LAGERAVBOK      PIC X(12).                                   
005600     03 RESP-FLLSBOK         PIC X.                                       
005700*                                 LAGERAVBOKNING                          
005800     03 RESP-BEGMRK-1        PIC X(30).                                   
005900     03 RESP-BEGMRK-2        PIC X(30).                                   
006000     03 RESP-BELAGINS-DEL1   PIC X(60).                                   
006100*                                 DEL AV LAGERINSTRUKTION                 
006200     03 RESP-BELAGINS-DEL2   PIC X(60).                                   
006300*                                 DEL AV LAGERINSTRUKTION                 
006400*** END OF VILMAII-COPY LENGTH= 523 BYTES                                 
