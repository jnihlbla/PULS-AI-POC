000100 01  W479A01.                                                             
000200*                                 PACKUNDERLAG, HISTORIK                  
000300*                                 HUVUD                                   
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001100*                                 PRODUKTIONSNUMMER                       
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001500*                                 KOLLINUMMER                             
001600     03 IDPURAD              PIC S9(5)           COMP-3.                  
001700*                                 RADNUMMER PÅ PACKUNDERLAG               
001800     03 IDKUNDRF             PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000     03 IDARTNR              PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200     03 KDORDKL              PIC S9              COMP-3.                  
002300*                                 ORDERKLASS                              
002400     03 IDLOTNR              PIC S9(3)           COMP-3.                  
002500*                                 VAGN-NUMMER                             
002600     03 KDORDLOT             PIC X(2).                                    
002700*                                 ORDERLOTTSALTERNATIV                    
002800     03 KDPERSON             PIC S9(3)           COMP-3.                  
002900*                                 PERSONKOD                               
003000     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003100*                                 FRAKTSÄTT DC TILL KUND                  
003200     03 KVORDRAD             PIC S9(5)           COMP-3.                  
003300*                                 ANTAL ORDERRADER                        
003400     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
003500*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
003600     03 TIORDREG             PIC S9(7)           COMP-3.                  
003700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003800     03 VLORDNTO             PIC S9(4)V9(3)      COMP-3.                  
003900*                                 ORDERVOLYM NETTO (M3)                   
004000     03 VKORDNTO             PIC S9(6)V9(1)      COMP-3.                  
004100*                                 ORDERVIKT NETTO (KG)                    
004200     03 BEVARREF             PIC X(10).                                   
004300*                                 VÅR REFERENS                            
004400     03 BEGMT.                                                            
004500*                                 GODSMOTTAGARNAMN                        
004600        05 BEGMT-RAD1        PIC X(35).                                   
004700*                                 GODSMOTTAGARNAMN RAD 1                  
004800        05 BEGMT-RAD2        PIC X(35).                                   
004900*                                 GODSMOTTAGARNAMN RAD 2                  
005000     03 ADGMT.                                                            
005100*                                 GODSMOTTAGARADRESS                      
005200        05 ADGMT-GATA        PIC X(35).                                   
005300*                                 GODSMOTTAGARADRESS GATA                 
005400        05 ADGMT-PADR        PIC X(35).                                   
005500*                                 GODSMOTTAGARADRESS POSTADRESS           
005600        05 ADGMT-LAND        PIC X(35).                                   
005700*                                 GODSMOTTAGARADRESS LAND                 
005800     03 BEGMRK.                                                           
005900*                                 GODSMÄRKE                               
006000        05 BEGMRK-RAD1       PIC X(30).                                   
006100*                                 GODSMÄRKE  RAD1                         
006200        05 BEGMRK-RAD2       PIC X(30).                                   
006300*                                 GODSMÄRKE  RAD2                         
006400     03 BELAGINS-GRP.                                                     
006500*                                 LAGERINSTRUKTIONER                      
006600        05 BELAGINS-DEL1     PIC X(60).                                   
006700*                                 DEL AV LAGERINSTRUKTION                 
006800        05 BELAGINS-DEL2     PIC X(60).                                   
006900*                                 DEL AV LAGERINSTRUKTION                 
007000     03 IDUSER               PIC X(8).                                    
007100*                                 ANVÄNDARENS SÄKERHETS ID                
007200*** END OF VILMAII-COPY LENGTH= 438 BYTES                                 
