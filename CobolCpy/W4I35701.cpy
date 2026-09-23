000100 01  MID-W4I35701.                                                        
000200     03 MID-IDDISTR-IN       PIC X(4).                                    
000300*                                 DISTRICT NUMBER                         
000400     03 MID-IDDISTR-OUT      PIC X(4).                                    
000500*                                 DISTRICT NUMBER                         
000600     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000700*                                 CUSTOMER NO                             
000800     03 MID-IDKUNDNR-OUT     PIC X(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 MID-IDORDNR7-IN      PIC X(7).                                    
001100*                                 ORDER NUMBER                            
001200     03 MID-IDORDNR7-OUT     PIC X(7).                                    
001300*                                 ORDER NUMBER                            
001400     03 MID-IDPRODNR-IN      PIC X(7).                                    
001500*                                 PRODUCTION-NUMBER                       
001600     03 MID-IDPRODNR-OUT     PIC X(7).                                    
001700*                                 PRODUCTION-NUMBER                       
001800     03 MID-IDDC-IN          PIC X(2).                                    
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 MID-IDDC-OUT         PIC X(2).                                    
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 MID-IDORDER-FIRST    PIC X(7).                                    
002300*                                 VOLVO PARTS ORDER NUMBER                
002400     03 MID-IDPRODNR-FIRST   PIC X(7).                                    
002500*                                 PRODUCTION-NUMBER                       
002600     03 MID-IDPLKLST-FIRST   PIC X(3).                                    
002700*                                 PICKING LIST NUMBER                     
002800     03 MID-IDORDER-ENTER    PIC X(7).                                    
002900*                                 VOLVO PARTS ORDER NUMBER                
003000     03 MID-IDPRODNR-ENTER   PIC X(7).                                    
003100*                                 PRODUCTION-NUMBER                       
003200     03 MID-IDPLKLST-ENTER   PIC X(3).                                    
003300*                                 PICKING LIST NUMBER                     
003400     03 MID-IDORDER-NEXT     PIC X(7).                                    
003500*                                 VOLVO PARTS ORDER NUMBER                
003600     03 MID-IDPRODNR-NEXT    PIC X(7).                                    
003700*                                 PRODUCTION-NUMBER                       
003800     03 MID-IDPLKLST-NEXT    PIC X(3).                                    
003900*                                 PICKING LIST NUMBER                     
004000     03 MID-IDPLKLST-RAD     OCCURS 12 TIMES                              
004100                             PIC 9(3).                                    
004200*                                 PICKING LIST NUMBER                     
004300     03 MID-IDPLKLST-UPD     PIC X(3).                                    
004400*                                 PICKING LIST NUMBER                     
004500     03 MID-TIAAMMDD-UPD     PIC 9(6).                                    
004600*                                 YEAR - MONTH - DAY  (YYMMDD)            
004700     03 MID-TIHHMM-UPD       PIC 9(4).                                    
004800*                                 TIME IN HOUR AND MINUTE                 
004900     03 MID-FLJANEJ          PIC X.                                       
