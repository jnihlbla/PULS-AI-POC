000100 01  DEC-WDECAREA.                                                        
000200*                                 PARAMETERAREA TILL WDECEDIT.            
000300*                                 NUMERISKT DATA KONVERTERAS FRÅN         
000400*                                 FRITT FORMAT TILL ETT FÖR COBOL         
000500*                                 MER HANTERLIGT FAST FORMAT.             
000600*                                 ----- INPARAMETRAR  ----------          
000700*                                 IDFRIDATA SKA INNEHÅLLA INDATA          
000800*                                 I FRITT FORMAT.  KVHELTAL OCH           
000900*                                 KVDECIMAL SKA INNEHÅLLA MAX             
001000*                                 TILLÅTET ANTAL HELTALSSIFFROR           
001100*                                 RESPEKTIVE DECIMALER.                   
001200*                                 ----  UTPARAMETRAR  -----------         
001300*                                 IDEDITDATA INNEHÅLLER DET               
001400*                                 NUMERISKA VÄRDET I REDIGERAT            
001500*                                 FORMAT.  KDSVAR = BLANK OM ALLT         
001600*                                 ÄR OK. = F OM DATAVÄRDET INTE           
001700*                                 VAR NUMERISKT ELLER FÖR MÅNGA           
001800*                                 HELTALSSIFFROR ELLER DECIMALER.         
001900*                                 DESSUTOM INNEHÅLLER KVHELTAL            
002000*                                 OCH KVDECIMAL DET VERKLIGA              
002100*                                 INMATADE ANTALET SIFFROR.               
002200     03 DEC-IDFRIDATA        PIC X(25).                                   
002300*                                 NUMERISKT DATA I FRITT FORMAT           
002400*                                 NUMERICAL DATA IN FREE FORMAT           
002500     03 DEC-IDEDITDATA       PIC S9(11)V9(4).                             
002600*                                 FAST FORMATERAT NUMERISKT DATA          
002700*                                 FIXED EDITED NUMERICAL DATA             
002800     03 DEC-KVHELTAL         PIC S9(4)           COMP.                    
002900*                                 ANTAL HELTALSSIFFROR                    
003000*                                 NUMBER OF INTEGER DIGITS                
003100     03 DEC-KVDECIMAL        PIC S9(4)           COMP.                    
003200*                                 ANTAL DECIMALSIFFROR                    
003300*                                 NUMBER OF DECIMAL DIGITS                
003400     03 DEC-KDSVAR           PIC X.                                       
003500      88 DEC-KDSVAR-OK       VALUE ' '.                                   
003600      88 DEC-KDSVAR-FEL      VALUE 'F'.                                   
003700*                                 SVARSKOD FRÅN SUBPROGRAM                
003800*                                                                         
003900*                                 RETURN CODE FROM SUBPROGRAM             
004000*                                                                         
004100*** END COPY WDECAREAC0  LENGTH=45                                        
