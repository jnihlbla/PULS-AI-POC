000100 01  MOD-W4O35701.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDDISTR-IN       PIC X(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001100*                                 ORDER NUMBER                            
001200     03 MOD-IDPRODNR-IN      PIC X(7).                                    
001300*                                 PRODUCTION-NUMBER                       
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 MOD-IDDISTR-OUT      PIC X(4).                                    
001700*                                 DISTRICT NUMBER                         
001800     03 MOD-IDKUNDNR-OUT     PIC X(6).                                    
001900*                                 CUSTOMER NO                             
002000     03 MOD-IDORDNR7-OUT     PIC X(7).                                    
002100*                                 ORDER NUMBER                            
002200     03 MOD-IDPRODNR-OUT     PIC X(7).                                    
002300*                                 PRODUCTION-NUMBER                       
002400     03 MOD-IDDC-OUT         PIC X(2).                                    
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 MOD-IDORDER-FIRST    PIC X(7).                                    
002700*                                 VOLVO PARTS ORDER NUMBER                
002800     03 MOD-IDPRODNR-FIRST   PIC X(7).                                    
002900*                                 PRODUCTION-NUMBER                       
003000     03 MOD-IDPLKLST-FIRST   PIC X(3).                                    
003100*                                 PICKING LIST NUMBER                     
003200     03 MOD-IDORDER-ENTER    PIC X(7).                                    
003300*                                 VOLVO PARTS ORDER NUMBER                
003400     03 MOD-IDPRODNR-ENTER   PIC X(7).                                    
003500*                                 PRODUCTION-NUMBER                       
003600     03 MOD-IDPLKLST-ENTER   PIC X(3).                                    
003700*                                 PICKING LIST NUMBER                     
003800     03 MOD-IDORDER-NEXT     PIC X(7).                                    
003900*                                 VOLVO PARTS ORDER NUMBER                
004000     03 MOD-IDPRODNR-NEXT    PIC X(7).                                    
004100*                                 PRODUCTION-NUMBER                       
004200     03 MOD-IDPLKLST-NEXT    PIC X(3).                                    
004300*                                 PICKING LIST NUMBER                     
004400     03 MOD-IDTRP-IN-ATTR    PIC X(2).                                    
004500     03 MOD-IDTRP-IN.                                                     
004600*                                 TRANSPORTIDENTITY                       
004700        05 MOD-IDTRPLOS      PIC X(3).                                    
004800*                                 TRANSPORTSOLUTION                       
004900        05 MOD-IDTRPVAR      PIC X(2).                                    
005000*                                 TRANSPORTSOLUTIONGROUP                  
005100     03 MOD-TITRPAVG-ATTR    PIC X(2).                                    
005200     03 MOD-TITRPAVG.                                                     
005300        05 MOD-TIAAMMDD      PIC 9(6).                                    
005400*                                 YEAR - MONTH - DAY  (YYMMDD)            
005500        05 MOD-FILLER        PIC X.                                       
005600        05 MOD-TIHHMM        PIC Z9.9(2).                                 
005700*                                 TIME IN HOUR AND MINUTE                 
005800     03 MOD-KDFRAKT-ATTR     PIC X(2).                                    
005900     03 MOD-KDFRAKT          PIC Z9.                                      
006000*                                 FREIGHT CODE                            
006100     03 MOD-IDKUNDRF-IHOP    PIC X(7).                                    
006200*                                 ORDER NUMBER                            
006300     03 MOD-LINE             OCCURS 12 TIMES.                             
006400        05 MOD-IDPLKLST-L-ATTR                                            
006500                             PIC X(2).                                    
006600        05 MOD-IDPLKLST      PIC X(3).                                    
006700*                                 PICKING LIST NUMBER                     
006800        05 MOD-IDPRC-L-ATTR  PIC X(2).                                    
006900        05 MOD-IDPRC.                                                     
007000*                                 PRODUCTION CHANNEL                      
007100           07 MOD-IDPRCBAS   PIC X(3).                                    
007200*                                 PRC-BASIC                               
007300           07 MOD-IDPRCVAR   PIC X.                                       
007400*                                 PRC-VARIANT                             
007500        05 MOD-KDODELSTA-L-ATTR                                           
007600                             PIC X(2).                                    
007700        05 MOD-KDODELSTA     PIC X.                                       
007800*                                 ORDER PART STATUS                       
007900        05 MOD-KVRADER-L-ATTR                                             
008000                             PIC X(2).                                    
008100        05 MOD-KVRADER       PIC Z(4)9.                                   
008200*                                 NUMBER OF LINES                         
008300        05 MOD-KVPACKRAD-OD-ATTR                                          
008400                             PIC X(2).                                    
008500        05 MOD-KVPACKRAD-OD  PIC Z(4)9.                                   
008600*                                 NUMBER OF PACKED RADER                  
008700        05 MOD-VKORDNTO-ATTR PIC X(2).                                    
008800        05 MOD-VKORDNTO      PIC Z(4)9.9.                                 
008900*                                 WEIGHT PER ORDER NETTO (KG)             
009000        05 MOD-VLORDNTO-ATTR PIC X(2).                                    
009100        05 MOD-VLORDNTO      PIC Z(2)9.9(3).                              
009200*                                 NET VOLUME PER ORDER (M3)               
009300        05 MOD-TILST-OD-ATTR PIC X(2).                                    
009400        05 MOD-TILST-OD.                                                  
009500           07 MOD-TIAAMMDD   PIC 9(6).                                    
009600*                                 YEAR - MONTH - DAY  (YYMMDD)            
009700           07 MOD-FILLER     PIC X.                                       
009800           07 MOD-TIHHMM     PIC Z9.9(2).                                 
009900*                                 TIME IN HOUR AND MINUTE                 
010000        05 MOD-SUPTID-ATTR   PIC X(2).                                    
010100        05 MOD-SUPTID        PIC Z(2)9.9(2).                              
010200*                                 TOTAL PRODUCTIONTIME HOUR MIN.          
010300        05 MOD-TIRFS-L-ATTR  PIC X(2).                                    
010400        05 MOD-TIRFS-OUT.                                                 
010500           07 MOD-TIAAMMDD   PIC 9(6).                                    
010600*                                 YEAR - MONTH - DAY  (YYMMDD)            
010700           07 MOD-FILLER     PIC X.                                       
010800           07 MOD-TIHHMM     PIC Z9.9(2).                                 
010900*                                 TIME IN HOUR AND MINUTE                 
011000        05 MOD-IDANSTNR-ATTR PIC X(2).                                    
011100        05 MOD-IDANSTNR      PIC X(5).                                    
011200*                                 IDENTIFICATION NO EMPLOYEE              
011300     03 MOD-IDPLKLST-ATTR    PIC X(2).                                    
011400     03 MOD-IDPLKLST-UPD     PIC X(2).                                    
011500*                                 MFS DISPOSITION OF INPUT FIELD          
011600     03 MOD-TIAAMMDD-ATTR    PIC X(2).                                    
011700     03 MOD-TIAAMMDD-UPD     PIC X(2).                                    
011800*                                 MFS DISPOSITION OF INPUT FIELD          
011900     03 MOD-TIHHMM-ATTR      PIC X(2).                                    
012000     03 MOD-TIHHMM-UPD       PIC X(2).                                    
012100*                                 MFS DISPOSITION OF INPUT FIELD          
012200     03 MOD-FLJANEJ-ATTR     PIC X(2).                                    
012300     03 MOD-FLJANEJ          PIC X(2).                                    
012400*                                 MFS DISPOSITION OF INPUT FIELD          
012500     03 MOD-TEMFSINF         PIC X(55).                                   
012600*                                 INFORMATION MESSAGE                     
