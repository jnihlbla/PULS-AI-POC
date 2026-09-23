000100 01  STR-WDECSTR.                                                         
000200*                                 PARAMETERAREA TILL WDECSTR.             
000300*                                 NUMERISKT DATA KONVERTERAS FRÅN         
000400*                                 FRITT FORMAT TILL ETT VALFRITT          
000500*                                 ZONAT FORMAT SOM EN STRÄNG.             
000600*                                 ----- INPARAMETRAR  ----------          
000700*                                 IDFRIDATA SKA INNEHÅLLA INDATA          
000800*                                 I FRITT FORMAT.  KVHELTAL OCH           
000900*                                 KVDECIMAL SKA INNEHÅLLA MAX             
001000*                                 TILLÅTET ANTAL HELTALSSIFFROR           
001100*                                 RESPEKTIVE DECIMALER.                   
001200*                                 ----  UTPARAMETRAR  -----------         
001300*                                 IDSTRDATA INNEHÅLLER DET                
001400*                                 NUMERISKA VÄRDET I REDIGERAT            
001500*                                 FORMAT.  KDSVAR = BLANK OM ALLT         
001600*                                 ÄR OK. = F OM DATAVÄRDET INTE           
001700*                                 VAR NUMERISKT ELLER FÖR MÅNGA           
001800*                                 HELTALSSIFFROR ELLER DECIMALER          
001900*                                 ELLER OM ETT ICKESIGNAT TAL             
002000*                                 INNEHÅLLER MINUSTECKEN.                 
002100     03 STR-IDFRIDATA        PIC X(25).                                   
002200*                                 NUMERISKT DATA I FRITT FORMAT           
002300*                                 NUMERICAL DATA IN FREE FORMAT           
002400     03 STR-IDSTRDATA        PIC X(15).                                   
002500*                                 NUMERISKT DATA SOM EN STRÄNG            
002600*                                 NUMERICAL DATA AS A STRING              
002700     03 STR-KVHELTAL         PIC S9(4)           COMP.                    
002800*                                 ANTAL HELTALSSIFFROR                    
002900*                                 NUMBER OF INTEGER DIGITS                
003000     03 STR-KVDECIMAL        PIC S9(4)           COMP.                    
003100*                                 ANTAL DECIMALSIFFROR                    
003200*                                 NUMBER OF DECIMAL DIGITS                
003300     03 STR-KDSIGNAT         PIC X.                                       
003400*                                 NUMERISKT DATA SIGNAT = Y               
003500*                                 NUMERICAL DATA WITH SIGN = Y            
003600     03 STR-KDLEFTJUST       PIC X.                                       
003700*                                 NUMERISKT DATA VÄNSTER-JUST = Y         
003800*                                 NUMERICAL DATA LEFT JUST = Y            
003900     03 STR-KDSVAR           PIC X.                                       
004000      88 STR-KDSVAR-OK       VALUE ' '.                                   
004100      88 STR-KDSVAR-FEL      VALUE 'F'.                                   
004200*                                                       KDSVAR-88         
004300*                                 SVARSKOD FRÅN SUBPROGRAM                
004400*                                                       KDSVAR-88         
004500*                                 RETURN CODE FROM SUBPROGRAM             
004600*** END COPY WDECSTR     LENGTH=47                                        
