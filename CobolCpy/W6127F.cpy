000100 01  W6127F.                                                              
000200*                                 DATA FOR REFILL FOLLOW-UP               
000300*                                 REPORTS                                 
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 TIAAVV               PIC S9(5)           COMP-3.                  
000800*                                 ÅR - VECKA  (ÅÅVV)                      
000900*                                 YEAR - WEEK  (YYWW)                     
001000     03 FLWEBDC              PIC X.                                       
001100*                                 DC MED WEB GRÄNSSNITT                   
001200*                                 DC WITH WEB INTERFACE                   
001300     03 KDMFUP               PIC X(2).                                    
001400*                                 RAPPORTGRUPP  MA/CN/PF/NA               
001500*                                 REPORT GROUP  MA/CN/PF/NA               
001600     03 IDLANDX2             PIC X(2).                                    
001700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001800*                                 2-LETTER CODE FOR COUNTRY               
001900     03 ADCITY               PIC X(25).                                   
002000*                                 BENÄMNING PÅ STAD                       
002100*                                 CITY                                    
002200     03 KDREFTYP             PIC X.                                       
002300*                                 TYP AV REFILLORDER                      
002400*                                 TYPE OF REFILLINGORDER                  
002500     03 KVANTAL-LINES-AK     PIC S9(7)           COMP-3.                  
002600*                                 ANTAL                                   
002700*                                 NUMBER                                  
002800     03 KVANTAL-LINES-BINNED PIC S9(7)           COMP-3.                  
002900*                                 ANTAL                                   
003000*                                 NUMBER                                  
003100     03 KVANTAL-LINES-PRIO   PIC S9(7)           COMP-3.                  
003200*                                 ANTAL                                   
003300*                                 NUMBER                                  
003400     03 KVDAGDEC-DAYS-BINNED PIC S9(4)V9(1)      COMP-3.                  
003500*                                 ANTAL DAGAR MED DECIMAL                 
003600     03 KVDAGDEC-DAYS-PRIO   PIC S9(4)V9(1)      COMP-3.                  
003700*                                 ANTAL DAGAR MED DECIMAL                 
003800     03 SUARTNTO-BINNED      PIC S9(9)V9(2)      COMP-3.                  
003900*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
004000*                                 TOTAL VALUE PER ITEM NETPRICE           
004100*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  
