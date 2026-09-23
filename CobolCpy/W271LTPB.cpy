000100 01  W271LTPB-W271LTPB.                                                   
000200*                                 LƒNKAREA TILL W271LTPB.                 
000300*                                                                         
000400*                                 SUBPROGRAMMET W271LTPB ANVƒNDS          
000500*                                 VID BERƒKNING AV LEDTID F÷R             
000600*                                 TRANSPORT CDC -> NDC                    
000700*                                                                         
000800*                                 SUBPROGRAMMET BERƒKNAR DELS             
000900*                                 VILKEN DAG B≈TGODS FINNS P≈             
001000*                                 HYLLA I NDC, DELS HUR M≈NGA             
001100*                                 ARBETSDAGAR (F÷RBRUKNINGSDAGAR)         
001200*                                 SOM FINNS FRAM TILL DESS                
001300*                                                                         
001400*                                 FYLL I INDATA:                          
001500*                                 IDDC         (OBLIGATORISKT)            
001600*                                 START-DATUM  (OBLIGATORISKT)            
001700*                                                                         
001800*                                                                         
001900     03 W271LTPB-IDDC        PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 W271LTPB-START-DATUM PIC S9(7)           COMP-3.                  
002200*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002300     03 W271LTPB-KVDAGAR-PER-I                                            
002400                             PIC S9(3)           COMP-3.                  
002500*                                 ANTAL DAGAR                             
002600     03 W271LTPB-KVDAGAR-PER-II                                           
002700                             PIC S9(3)           COMP-3.                  
002800*                                 ANTAL DAGAR                             
002900     03 W271LTPB-KVDAGAR-PER-III                                          
003000                             PIC S9(3)           COMP-3.                  
003100*                                 ANTAL DAGAR                             
003200     03 W271LTPB-KVDAGAR-PER-IV                                           
003300                             PIC S9(3)           COMP-3.                  
003400*                                 ANTAL DAGAR                             
003500     03 W271LTPB-KVDAGAR-PER-V                                            
003600                             PIC S9(3)           COMP-3.                  
003700*                                 ANTAL DAGAR                             
003800     03 W271LTPB-KVDAGAR-PER-VI                                           
003900                             PIC S9(3)           COMP-3.                  
004000*                                 ANTAL DAGAR                             
004100     03 W271LTPB-KVDLTID-TOT PIC S9(3)           COMP-3.                  
004200*                                 ANTAL DAGAR                             
004300     03 W271LTPB-KVDLTID-BOATPAC                                          
004400                             PIC S9(3)           COMP-3.                  
004500*                                 ANTAL DAGAR                             
004600     03 W271LTPB-KVDLTID-BOATTRP                                          
004700                             PIC S9(3)           COMP-3.                  
004800*                                 ANTAL DAGAR                             
004900     03 W271LTPB-KVDLTID-BOAT2DC                                          
005000                             PIC S9(3)           COMP-3.                  
005100*                                 ANTAL DAGAR                             
005200     03 W271LTPB-KVDLTID-BOATINS                                          
005300                             PIC S9(3)           COMP-3.                  
005400*                                 ANTAL DAGAR                             
005500     03 W271LTPB-KVDLTID-AIRREQ                                           
005600                             PIC S9(3)           COMP-3.                  
005700*                                 ANTAL DAGAR                             
005800     03 W271LTPB-KVDLTID-AIRETA                                           
005900                             PIC S9(3)           COMP-3.                  
006000*                                 ANTAL DAGAR                             
006100     03 W271LTPB-PER-I-TIRP  PIC S9(2)           COMP-3.                  
006200*                                 REDOVISNINGSPERIOD                      
006300*                                 12 PER ≈R                               
006400     03 W271LTPB-PER-II-TIRP PIC S9(2)           COMP-3.                  
006500*                                 REDOVISNINGSPERIOD                      
006600*                                 12 PER ≈R                               
006700     03 W271LTPB-PER-III-TIRP                                             
006800                             PIC S9(2)           COMP-3.                  
006900*                                 REDOVISNINGSPERIOD                      
007000*                                 12 PER ≈R                               
007100     03 W271LTPB-PER-IV-TIRP PIC S9(2)           COMP-3.                  
007200*                                 REDOVISNINGSPERIOD                      
007300*                                 12 PER ≈R                               
007400     03 W271LTPB-PER-V-TIRP  PIC S9(2)           COMP-3.                  
007500*                                 REDOVISNINGSPERIOD                      
007600*                                 12 PER ≈R                               
007700     03 W271LTPB-PER-VI-TIRP PIC S9(2)           COMP-3.                  
007800*                                 REDOVISNINGSPERIOD                      
007900*                                 12 PER ≈R                               
008000     03 W271LTPB-PER-I-TIAARP                                             
008100                             PIC S9(5)           COMP-3.                  
008200*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
008300*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
008400     03 W271LTPB-PER-II-TIAARP                                            
008500                             PIC S9(5)           COMP-3.                  
008600*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
008700*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
008800     03 W271LTPB-PER-III-TIAARP                                           
008900                             PIC S9(5)           COMP-3.                  
009000*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
009100*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
009200     03 W271LTPB-PER-IV-TIAARP                                            
009300                             PIC S9(5)           COMP-3.                  
009400*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
009500*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
009600     03 W271LTPB-PER-V-TIAARP                                             
009700                             PIC S9(5)           COMP-3.                  
009800*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
009900*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
010000     03 W271LTPB-PER-VI-TIAARP                                            
010100                             PIC S9(5)           COMP-3.                  
010200*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
010300*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
010400     03 W271LTPB-BINNDAY-TIAAMMDD                                         
010500                             PIC S9(7)           COMP-3.                  
010600*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
010700     03 W271LTPB-BINNDAY-TIRP                                             
010800                             PIC S9(2)           COMP-3.                  
010900*                                 REDOVISNINGSPERIOD                      
011000*                                 12 PER ≈R                               
011100     03 W271LTPB-IDDC-REF    PIC X(2).                                    
011200*                                 SƒNDANDE LAGER F÷R REFILL               
011300*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
