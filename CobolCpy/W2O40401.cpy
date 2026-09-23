000100 01  MOD-W2O40401.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 PART NUMBER                             
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 PART NUMBER                             
001200     03 MOD-HYPHEN           PIC X.                                       
001300     03 MOD-REKSIFFR         PIC 9.                                       
001400*                                 PART NO CHECK DIGIT                     
001500     03 MOD-BEART            PIC X(25).                                   
001600*                                 PART DESCRIPTION                        
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 MOD-FILLER           OCCURS 12 TIMES.                             
002000        05 MOD-TIAARP        PIC X(4).                                    
002100*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002200*                                 12 PER YEAR, ALSO LOGISTICS PER         
002300     03 MOD-FILLER           OCCURS 12 TIMES.                             
002400        05 MOD-KVVIPER       PIC 9.                                       
002500*                                 NUMBER OF WEEKS IN A PERIOD             
002600     03 MOD-FILLER.                                                       
002700        05 MOD-KVOI-DC       OCCURS 12 TIMES                              
002800                             PIC Z(5)9.                                   
002900*                                 ORDERED PCS PER TIME UNIT               
003000        05 MOD-KVOT-OH       OCCURS 12 TIMES                              
003100                             PIC Z(5)9.                                   
003200*                                 NO OF ORDERHITS                         
003300        05 MOD-KVOI-OTH      OCCURS 12 TIMES                              
003400                             PIC Z(5)9.                                   
003500*                                 ORDERED PCS PER TIME UNIT               
003600     03 MOD-WEEK-IN-PERIOD   PIC X(6).                                    
003700     03 MOD-KVOI-DC-TOT      PIC Z(5)9.                                   
003800*                                 ORDERED PCS PER TIME UNIT               
003900     03 MOD-KVOT-OH-TOT      PIC Z(5)9.                                   
004000*                                 NO OF ORDERHITS                         
004100     03 MOD-KVOI-OTH-TOT     PIC Z(5)9.                                   
004200*                                 ORDERED PCS PER TIME UNIT               
004300     03 MOD-FILLER           OCCURS 3 TIMES.                              
004400        05 MOD-YEAR          PIC 9(2).                                    
004500*                                 YEAR  (YY)                              
004600        05 MOD-YEAR-DC       PIC Z(6).                                    
004700        05 MOD-YEAR-OH       PIC Z(6).                                    
004800        05 MOD-YEAR-OTH      PIC Z(6).                                    
004900     03 MOD-FILLER.                                                       
005000        05 MOD-LATE-12-DC    PIC Z(6).                                    
005100        05 MOD-LATE-12-OH    PIC Z(6).                                    
005200        05 MOD-LATE-12-OTH   PIC Z(6).                                    
005300     03 MOD-FILLER.                                                       
005400        05 MOD-LATE-6-DC     PIC Z(6).                                    
005500        05 MOD-LATE-6-OH     PIC Z(6).                                    
005600        05 MOD-LATE-6-OTH    PIC Z(6).                                    
005700     03 MOD-FILLER.                                                       
005800        05 MOD-AVER-12-DC    PIC Z(4)9.9.                                 
005900        05 MOD-AVER-12-OTH   PIC Z(4)9.9.                                 
006000     03 MOD-FILLER.                                                       
006100        05 MOD-AVER-6-DC     PIC Z(4)9.9.                                 
006200        05 MOD-AVER-6-OTH    PIC Z(4)9.9.                                 
006300     03 MOD-FILLER.                                                       
006400        05 MOD-KVPB-SEP-DC-ATTR                                           
006500                             PIC X(2).                                    
006600        05 MOD-KVPB-SEP-DC   PIC Z(5)9.9.                                 
006700*                                 SEPARATE PERIOD REQUIREMENTS            
006800        05 MOD-KVPBREOI-DC-ATTR                                           
006900                             PIC X(2).                                    
007000        05 MOD-KVPBREOI-DC   PIC Z(5)9.9.                                 
007100*                                 PERIOD REQUIREM. REFILLING OI           
007200        05 MOD-KVPB-SEP-OTH  PIC Z(5)9.9.                                 
007300*                                 SEPARATE PERIOD REQUIREMENTS            
007400     03 MOD-KVPB-DC-IN-ATTR  PIC X(2).                                    
007500     03 MOD-KVPB-DC-IN       PIC Z(5)9.9.                                 
007600*                                 SEPARATE PERIOD REQUIREMENTS            
007700     03 MOD-KVPBREOI-DC-IN-ATTR                                           
007800                             PIC X(2).                                    
007900     03 MOD-KVPBREOI-DC-IN   PIC Z(5)9.9.                                 
008000*                                 PERIOD REQUIREM. REFILLING OI           
008100     03 MOD-TIREFMPB-ATTR    PIC X(2).                                    
008200     03 MOD-TIREFMPB         PIC 9(6).                                    
008300*                                 DATE MANUAL FORECAST REFILLING          
008400     03 MOD-TIREFMPB-IN-ATTR PIC X(2).                                    
008500     03 MOD-TIREFMPB-IN      PIC X(6).                                    
008600*                                 DATE MANUAL FORECAST REFILLING          
008700     03 MOD-DAREFESC-ATTR    PIC X(2).                                    
008800     03 MOD-DAREFESC         PIC X(6).                                    
008900*                                 YEAR - MONTH - DAY  (YYMMDD)            
009000     03 MOD-DAREFESC-IN-ATTR PIC X(2).                                    
009100     03 MOD-DAREFESC-IN      PIC X(6).                                    
009200*                                 YEAR - MONTH - DAY  (YYMMDD)            
009300     03 MOD-KVPB-PLAN-ATTR   PIC X(2).                                    
009400     03 MOD-KVPB-PLAN        PIC Z(5)9.9.                                 
009500*                                 PLANNED PERIOD REQUIREMENTS             
009600     03 MOD-DAPBPLAN-ATTR    PIC X(2).                                    
009700     03 MOD-DAPBPLAN         PIC X(6).                                    
009800*                                 YEAR - MONTH - DAY  (YYMMDD)            
009900     03 MOD-KVPB-PLAN-IN-ATTR                                             
010000                             PIC X(2).                                    
010100     03 MOD-KVPB-PLAN-IN     PIC Z(5)9.9.                                 
010200*                                 PLANNED PERIOD REQUIREMENTS             
010300     03 MOD-DAPBPLAN-IN-ATTR PIC X(2).                                    
010400     03 MOD-DAPBPLAN-IN      PIC X(6).                                    
010500*                                 YEAR - MONTH - DAY  (YYMMDD)            
010600     03 MOD-KVPB-TREND       PIC +(6)9.9.                                 
010700     03 MOD-TEMFSINF         PIC X(55).                                   
010800*                                 INFORMATION MESSAGE                     
010900*** END OF VILMAII-COPY LENGTH= 697 BYTES                                 
