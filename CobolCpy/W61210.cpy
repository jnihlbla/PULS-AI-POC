000100 01  W61210.                                                              
000200*                                 URVAL FR≈N WDL6                         
000300*                                 INLEVERANS HISTORIK SDC                 
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 DAINLEV              PIC 9(16).                                   
000900*                                 INLEVERANS NUMMER                       
001000     03 IDFAKT               PIC S9(7)           COMP-3.                  
001100*                                 FAKTURANUMMER                           
001200     03 IDPTYP               PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001500*                                 KOLLINUMMER                             
001600     03 ADART.                                                            
001700*                                 ARTIKELADRESS I LAGRET                  
001800        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
001900*                                 LAGEROMR≈DE                             
002000        05 ADGANG            PIC S9(3)           COMP-3.                  
002100*                                 G≈NG                                    
002200        05 ADPLATS           PIC S9(5)           COMP-3.                  
002300*                                 LAGERPLATSNUMMER                        
002400     03 IDGMTREF.                                                         
002500*                                 GODSMOTTAGAREREFERENS                   
002600        05 IDDISTR           PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
002900*                                 KUNDNUMMER                              
003000        05 IDKUNDRF-GRP.                                                  
003100*                                 KUNDENS REFERENS (ORDERID)              
003200           07 IDKUNDRF       PIC X(10).                                   
003300*                                 KUNDENS REFERENS (ORDERID)              
003400           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
003500              09 IDORDNR5    PIC 9(5).                                    
003600*                                 ORDERNUMMER                             
003700              09 FILLER      PIC X(5).                                    
003800           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
003900              09 IDORDNR7    PIC 9(7).                                    
004000*                                 ORDERNUMMER                             
004100              09 FILLER      PIC X(3).                                    
004200     03 KDKOLLI              PIC X(8).                                    
004300*                                 KOLLIKOD                                
004400     03 KVANTMOT             PIC S9(7)           COMP-3.                  
004500*                                 ANTAL MOTTAGET                          
004600     03 KVAVIS               PIC S9(7)           COMP-3.                  
004700*                                 AVISERAT ANTAL                          
004800     03 TIINLMOT             PIC S9(7)           COMP-3.                  
004900*                                 MOTTAGNINGSDATUM   (≈≈MMDD)             
005000     03 TIINLINL             PIC S9(7)           COMP-3.                  
005100*                                 RAPPORTERINGSDATUM INLAGD (R32)         
005200*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
