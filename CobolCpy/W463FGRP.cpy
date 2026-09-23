000001*** EDIT ALLOWED                                                          
000002*                            *************************************        
000003*                            *** ANVÄNDS FÖR ATT öVERSÄTTA                
000004*                            *** PRODTYP TILL FUNKTIONSGRUPP              
000005*                            *** I TRANSAR FÖR DIRECT-BUSINESS.           
000006*                            ***                                          
000007*                            *************************************        
000008*                                                                         
000009*   TABELL FÖR ATT SÖKA FUNKTIONSGRUPP.                                   
000010*                                                                         
000011 01  PTYP-FGRP-VALUES.                                                    
000012***********************************PROGPFGRP                              
000013     03  FILLER    PIC X(9) VALUE 'VCONA1009'.                            
000014     03  FILLER    PIC X(9) VALUE 'VCONB1819'.                            
000015     03  FILLER    PIC X(9) VALUE 'VCONC1819'.                            
000016     03  FILLER    PIC X(9) VALUE 'VCON01819'.                            
000017     03  FILLER    PIC X(9) VALUE 'VCON10811'.                            
000018     03  FILLER    PIC X(9) VALUE 'VCON21832'.                            
000019     03  FILLER    PIC X(9) VALUE 'VCON31009'.                            
000020     03  FILLER    PIC X(9) VALUE 'VCON61819'.                            
000030     03  FILLER    PIC X(9) VALUE 'VMERA9999'.                            
000040     03  FILLER    PIC X(9) VALUE 'VMERB9999'.                            
000050     03  FILLER    PIC X(9) VALUE 'VMERC9999'.                            
000060     03  FILLER    PIC X(9) VALUE 'VMER09999'.                            
000070     03  FILLER    PIC X(9) VALUE 'VMER69999'.                            
000080     03  FILLER    PIC X(9) VALUE 'VMER99999'.                            
000090     03  FILLER    PIC X(9) VALUE 'VSER69398'.                            
000091     03  FILLER    PIC X(9) VALUE 'VSER79991'.                            
000100     03  FILLER    PIC X(9) VALUE 'VTYRA7797'.                            
000200     03  FILLER    PIC X(9) VALUE 'VTYRB7798'.                            
000300     03  FILLER    PIC X(9) VALUE 'VTYRC7796'.                            
000400     03  FILLER    PIC X(9) VALUE 'VTYRE7725'.                            
000500     03  FILLER    PIC X(9) VALUE 'VTYRF7703'.                            
000600     03  FILLER    PIC X(9) VALUE 'VTYRG7702'.                            
000700     03  FILLER    PIC X(9) VALUE 'VTYRH7701'.                            
000800     03  FILLER    PIC X(9) VALUE 'VTYR17726'.                            
000900     03  FILLER    PIC X(9) VALUE 'VTYR27724'.                            
001000     03  FILLER    PIC X(9) VALUE 'VTYR30000'.                            
001100     03  FILLER    PIC X(9) VALUE 'VTYR47729'.                            
001200     03  FILLER    PIC X(9) VALUE 'VTYR57729'.                            
001300     03  FILLER    PIC X(9) VALUE 'VTYR60000'.                            
001400     03  FILLER    PIC X(9) VALUE 'VTYR70000'.                            
001500     03  FILLER    PIC X(9) VALUE 'VTYR87729'.                            
001600     03  FILLER    PIC X(9) VALUE 'VTYR97729'.                            
001700*                                                                         
001800 01  PTYP-FGRP-TAB        REDEFINES PTYP-FGRP-VALUES.                     
001900     03  PTYP-FGRP-ING    OCCURS 32 TIMES                                 
002000                          ASCENDING KEY IS PTYP-SOK                       
002100                          INDEXED BY PTYP-IX.                             
002200       05  PTYP-SOK            PIC X(5).                                  
002300       05  PTYP-FGRP           PIC 9(4).                                  
002400*                                                                         
002500*                                                                         
002600*** END COPY W463FGRP    LENGTH=144                                       
