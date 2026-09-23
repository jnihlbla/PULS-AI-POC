000010*** EDIT ALLOWED                                                          
000100 01  W23365.                                                              
000200*                                                                         
000300*                                       VECKOTRANSAR TILL CARPAC          
000310*                                     ********                            
000320*                                     * VPUP *                            
000400*                                     ********                            
000500*                                                                         
000600    03  FZ2RECT            PIC  X(04).                                    
000700*                                          RECORD TYPE                    
000800    03  FZ2PART.                                                          
000900      05  IDARTNR          PIC  9(07).                                    
001100      05  FILLER           PIC  X(05).                                    
001110*                                                                         
001120    03  FZ2PART-8          REDEFINES FZ2PART.                             
001130      05  IDARTNR-8        PIC  9(08).                                    
001150      05  FILLER           PIC  X(04).                                    
001200*                                          PART NUMBER                    
001210*                                                                         
001300    03  FZ2SALY            PIC  9(07)-.                                   
001400*                                          SALES LAST YEAR                
001500*                                          (SUARTFSG-FAAR)                
001510*                                                                         
001600    03  FZ2SYTD            PIC  9(07)-.                                   
001700*                                          SALES YEAR TO DATE             
001800*                                          (SUARTFSG-AAR)                 
001810*                                                                         
002200    03  FZ2FORC            PIC  9(07)-.                                   
002300*                                          FORECAST                       
002400*                                          (KVPB-TOT * 4.3/6)             
002500*                                                                         
002600    03  FZ2TRND            PIC  9V99-.                                    
002700*                                          TREND                          
002800*                                          ((KVUTJFEL/                    
002900*                                          KVMAD-TOT) * -1)               
003000*                                                                         
003100    03  FZ2SUPB            PIC  9(09)-.                                   
003200*                                          SUPERBALANCE                   
003300*                                          (KVLS + KVBR -KVROS)           
003400*                                                                         
004300    03  FZ2FOBL            PIC  9(07)V99-.                                
004400*                                          FOB/SEL PRICE                  
004500*                                          (PRARTBTO-MARK)                
004600*                                                                         
004700    03  FZ2STCK            PIC  9(09)-.                                   
004800*                                          STOCK                          
004900*                                          (KVLS)                         
005000*                                                                         
005100    03  FZ2XANL            PIC  X(02).                                    
005200*                                          ANALYST CODE                   
005300*                                          (IDANSK (99.))                 
005400*                                                                         
005500    03  FZ2SSCD            PIC  X(02).                                    
005600*                                          SUPERSESSION CODE              
005700*                                          (KDERS)                        
005800*                                                                         
005900    03  FZ2DELD            PIC   9(06).                                   
006000*                                          YYMMDD, MOST RE-               
006100*                                          CENT DELIVERY DATE             
006200*                                          (TIAVIDAT-SEN)                 
006300*                                                                         
006400    03  FZ2PREM.                                                          
006500*                                          PROCUREMENT REMARK             
006600      05 TEARTNOT-UT-1     PIC  X(40).                                    
006700      05 TEARTNOT-UT-2     PIC  X(20).                                    
006800*                                                                         
006900    03  FZ2NDEM            PIC  9(07)-.                                   
007000*                                          NORMAL DEMAND                  
007100*                                          CURRENT WEEK                   
007200*                                          (KVOI 11,14,21,24)             
007300*                                                                         
007400    03  FZ2SDEM            PIC  9(07)-.                                   
007500*                                          SPECIAL DEMAND                 
007600*                                          CURRENT WEEK                   
007700*                                          (KVOI 13,23)                   
007800*                                                                         
007900    03  FZ2DEMD            PIC 9(06).                                     
008000*                                          YYMMDD, LAST DATE              
008100*                                          CURR.WK FOR DEM/DEL            
008300*                                          (SÄNDNINGSDATUM)               
008400*                                                                         
008500    03  FZ2DELI            PIC  9(07)-.                                   
008600*                                          CUMULATED DELIVERY             
008700*                                          CURRENT WEEK                   
008800*                                          (FAKTISK INLEV V)              
008900*                                                                         
009000    03  FZ2ONOR            PIC  9(07)-.                                   
009100*                                          ON ORDER                       
009200*                                          (KVBR)                         
009300*                                                                         
009400    03  FZ2CALL            PIC  9(07)-.                                   
009500*                                          CUMULATED CALL-                
009600*                                          OFFS CURR WEEK                 
009700*                                          (KVAVROP)                      
009800*                                                                         
009900    03  FZ2AGEC            PIC  X(02).                                    
010000*                                          AGECODE                        
010100*                                          (KDAGE)                        
010200*                                                                         
010300    03  FZ2FILLER          PIC  X(08).                                    
010400*                                          FILLER                         
010500***END COPY W23365   LENTH=200                                            
