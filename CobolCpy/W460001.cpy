000100 01  OHUV-W460001.                                                        
000200*                                 ORDERHUVUD-TRANSAKTIONER NOAC           
000300*                                 POSTTYP = RHA                           
000400     03 OHUV-SORT-IDDISTR    PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 OHUV-SORT-TIFILDAT   PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 OHUV-SORT-TIHHMMSS   PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 OHUV-IDPTYP          PIC X(3).                                    
001100*                                 POSTTYP                                 
001200     03 OHUV-IDDISTR         PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 OHUV-IDKUNDNR        PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 OHUV-IDORDNR         PIC 9(7).                                    
001700*                                 ORDERNR             IDORDNR-002         
001800     03 OHUV-BEVOLREF        PIC X(10).                                   
001900*                                 VOLVO REFERENS                          
002000     03 OHUV-KDFRAKT         PIC 9(2).                                    
002100*                                 FRAKTSÄTT DC TILL KUND                  
002200     03 OHUV-KDORDKL         PIC 9.                                       
002300*                                 ORDERKLASS                              
002400     03 OHUV-KDORDKL-IMP     PIC 9.                                       
002500*                                 ORDERKLASS FRÅN IMPORTÖREN              
002600     03 OHUV-KDROPACK        PIC X.                                       
002700*                                 FRISLÄPPNINGSKOD RO/DO                  
002800     03 OHUV-KDORDURS        PIC X.                                       
002900*                                 OREDR URSPRUNG                          
003000     03 OHUV-BEVARREF        PIC X(10).                                   
003100*                                 VÅR REFERENS                            
003200     03 OHUV-FLRESTN         PIC X.                                       
003300*                                 RESTNOTERING ?                          
003400     03 OHUV-KDNCNOT         PIC X(2).                                    
003500*                                 NOTERINGSKOD ORDER IN                   
003600     03 OHUV-KDTPOTYP        PIC 9.                                       
003700*                                 TYP AV TIDPLANERAD ORDER                
003800     03 OHUV-IDKAMPRF        PIC 9(7).                                    
003900*                                 KAMPANJREFERENS                         
004000     03 OHUV-BEGMT.                                                       
004100*                                 GODSMOTTAGARNAMN                        
004200        05 OHUV-BEGMT-RAD1   PIC X(35).                                   
004300*                                 GODSMOTTAGARNAMN RAD 1                  
004400        05 OHUV-BEGMT-RAD2   PIC X(35).                                   
004500*                                 GODSMOTTAGARNAMN RAD 2                  
004600     03 OHUV-ADGMT.                                                       
004700*                                 GODSMOTTAGARADRESS                      
004800        05 OHUV-ADGMT-GATA   PIC X(35).                                   
004900*                                 GODSMOTTAGARADRESS GATA                 
005000        05 OHUV-ADGMT-PADR   PIC X(35).                                   
005100*                                 GODSMOTTAGARADRESS POSTADRESS           
005200        05 OHUV-ADPOST-PNRORT REDEFINES OHUV-ADGMT-PADR.                  
005300*                                 POSTNUMMER + ORT                        
005400           07 OHUV-ADPOSTNR  PIC X(10).                                   
005500*                                 POSTNUMMER I ADRESS                     
005600           07 OHUV-ADCITY    PIC X(25).                                   
005700*                                 BENÄMNING PÅ STAD                       
005800        05 OHUV-ADPOST-ORTPNR REDEFINES OHUV-ADGMT-PADR.                  
005900*                                 ORT + POSTNUMMER                        
006000           07 OHUV-ADCITY    PIC X(25).                                   
006100*                                 BENÄMNING PÅ STAD                       
006200           07 OHUV-ADPOSTNR  PIC X(10).                                   
006300*                                 POSTNUMMER I ADRESS                     
006400        05 OHUV-ADGMT-LAND   PIC X(35).                                   
006500*                                 GODSMOTTAGARADRESS LAND                 
006600     03 OHUV-TIBEGPAC        PIC 9(6).                                    
006700*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
006800     03 OHUV-FILLER          PIC X.                                       
006900     03 OHUV-IDTRANSLOP      PIC 9(5).                                    
007000*                                 TRANSAKTIONS-LÖPNUMMER                  
007100     03 OHUV-KDFEL           PIC 9(3).                                    
007200*                                 FELKOD                                  
007300     03 OHUV-IDBILREG        PIC X(10).                                   
007400*                                 BILENS REGISTRERINGSNUMMER              
007500*** END OF VILMAII-COPY LENGTH= 273 BYTES                                 
