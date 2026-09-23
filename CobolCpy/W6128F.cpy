000100 01  W6128F.                                                              
000200*                                 DATA FOR REFILL FOLLOW-UP               
000300*                                 REPORTS                                 
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 TIAARP               PIC S9(5)           COMP-3.                  
000800*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
000900*                                 12 PER ÅR (OCKSÅ LOGISTIKPER)           
001000*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
001100*                                 12 PER YEAR, ALSO LOGISTICS PER         
001200     03 FLWEBDC              PIC X.                                       
001300*                                 DC MED WEB GRÄNSSNITT                   
001400*                                 DC WITH WEB INTERFACE                   
001500     03 KDMFUP               PIC X(2).                                    
001600*                                 RAPPORTGRUPP  MA/CN/PF/NA               
001700*                                 REPORT GROUP  MA/CN/PF/NA               
001800     03 IDLANDX2             PIC X(2).                                    
001900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002000*                                 2-LETTER CODE FOR COUNTRY               
002100     03 ADCITY               PIC X(25).                                   
002200*                                 BENÄMNING PÅ STAD                       
002300*                                 CITY                                    
002400     03 KDREFTYP             PIC X.                                       
002500*                                 TYP AV REFILLORDER                      
002600*                                 TYPE OF REFILLINGORDER                  
002700     03 KVANTAL-LINES-AK     PIC S9(7)           COMP-3.                  
002800*                                 ANTAL                                   
002900*                                 NUMBER                                  
003000     03 KVANTAL-LINES-BINNED PIC S9(7)           COMP-3.                  
003100*                                 ANTAL                                   
003200*                                 NUMBER                                  
003300     03 KVANTAL-LINES-PRIO   PIC S9(7)           COMP-3.                  
003400*                                 ANTAL                                   
003500*                                 NUMBER                                  
003600     03 KVDAGDEC-DAYS-BINNED PIC S9(4)V9(1)      COMP-3.                  
003700*                                 ANTAL DAGAR MED DECIMAL                 
003800     03 KVDAGDEC-DAYS-PRIO   PIC S9(4)V9(1)      COMP-3.                  
003900*                                 ANTAL DAGAR MED DECIMAL                 
004000     03 SUARTNTO-BINNED      PIC S9(9)V9(2)      COMP-3.                  
004100*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
004200*                                 TOTAL VALUE PER ITEM NETPRICE           
004300*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
