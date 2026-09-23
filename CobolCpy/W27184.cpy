000100 01  W27184.                                                              
000200*                                 INFORMATION FROM WDL6                   
000300*                                 INBOUND HISTORY                         
000400*                                 COPY OF W61210, ADDED SENDING D         
000500*                                 C                                       
000600     03 IDDC-RECV            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDDC-SEND            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 DAINLEV              PIC 9(16).                                   
001300*                                 INLEVERANS NUMMER                       
001400     03 IDFAKT               PIC S9(7)           COMP-3.                  
001500*                                 FAKTURANUMMER                           
001600     03 IDPTYP               PIC X(3).                                    
001700*                                 POSTTYP                                 
001800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000     03 ADART.                                                            
002100*                                 ARTIKELADRESS I LAGRET                  
002200        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMR≈DE                             
002400        05 ADGANG            PIC S9(3)           COMP-3.                  
002500*                                 G≈NG                                    
002600        05 ADPLATS           PIC S9(5)           COMP-3.                  
002700*                                 LAGERPLATSNUMMER                        
002800     03 IDGMTREF.                                                         
002900*                                 GODSMOTTAGAREREFERENS                   
003000        05 IDDISTR           PIC S9(5)           COMP-3.                  
003100*                                 DISTRIKTNUMMER                          
003200        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
003300*                                 KUNDNUMMER                              
003400        05 IDKUNDRF-GRP.                                                  
003500*                                 KUNDENS REFERENS (ORDERID)              
003600           07 IDKUNDRF       PIC X(10).                                   
003700*                                 KUNDENS REFERENS (ORDERID)              
003800           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
003900              09 IDORDNR5    PIC 9(5).                                    
004000*                                 ORDERNUMMER                             
004100              09 FILLER      PIC X(5).                                    
004200           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
004300              09 IDORDNR7    PIC 9(7).                                    
004400*                                 ORDERNUMMER                             
004500              09 FILLER      PIC X(3).                                    
004600     03 KDKOLLI              PIC X(8).                                    
004700*                                 KOLLIKOD                                
004800     03 KVANTMOT             PIC S9(7)           COMP-3.                  
004900*                                 ANTAL MOTTAGET                          
005000     03 KVAVIS               PIC S9(7)           COMP-3.                  
005100*                                 AVISERAT ANTAL                          
005200     03 TIINLMOT             PIC S9(7)           COMP-3.                  
005300*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
005400     03 TIINLINL             PIC S9(7)           COMP-3.                  
005500*                                 RAPPORTERINGSDATUM INLAGD (R32)         
005600*** END OF VILMAII-COPY LENGTH= 83 BYTES                                  
