000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W475BEST.                                                
000400*AUTHOR.         GERRY CARMICHAEL.                                        
000500*DATE-WRITTEN.   93/04/02.                                                
000600                                                                          
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        SUBPGM SOM HÄMTAR BESTÄMMELSELAND FÖR                            
001200*        ANGIVEN DISTRIKT.                                                
001300*                                                                         
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     EJECT                                                                
001700 DATA DIVISION.                                                           
001800     SKIP3                                                                
001900 WORKING-STORAGE SECTION.                                                 
002000     SKIP2                                                                
002100                                                                          
002200*    -- CHECKED BY WY2000                                                 
002300 77  IDPGM                       PIC X(8)    VALUE 'W475BEST'.            
002400 77  JA                          PIC X       VALUE 'J'.                   
002500 77  NEJ                         PIC X       VALUE 'N'.                   
002600*                                                                         
002700 01  W-IDDISTR                   PIC 9(5)    VALUE ZERO.                  
002800                                                                          
002900 01  W-IDDISTR-RED-3.                                                     
003000   03  FILLER                    PIC 9(1)    VALUE ZERO.                  
003100   03  W-IDDISTR-3               PIC 9(3)    VALUE ZERO.                  
003200   03  FILLER                    PIC 9(1)    VALUE ZERO.                  
003300                                                                          
003400 01  W-IDDISTR-RED-2.                                                     
003500   03  FILLER                    PIC 9(1)    VALUE ZERO.                  
003600   03  W-IDDISTR-2               PIC 9(2)    VALUE ZERO.                  
003700   03  FILLER                    PIC 9(2)    VALUE ZERO.                  
003800                                                                          
003900 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
004000     SKIP3                                                                
004100*01  FILLER   -COPY WWDIST03    -RED TEST-IDDISTR.                        
004200     EJECT                                                                
004300 LINKAGE SECTION.                                                         
004400                                                                          
004500*01  -COPY W475BEST                                                       
004600     EJECT                                                                
004700 PROCEDURE DIVISION  USING BEST-W475BEST.                                 
004800     SKIP2                                                                
004900                                                                          
005000     MOVE BEST-IDDISTR      TO W-IDDISTR                                  
005100                               TEST-IDDISTR                               
005200     MOVE W-IDDISTR         TO W-IDDISTR-RED-3                            
005300                               W-IDDISTR-RED-2                            
005400     EVALUATE TRUE                                                        
005500                                                                          
005600       WHEN DIST03-SVERIGE                                                
005700         MOVE 'SE ' TO BEST-IDLANDX3                                      
005800       WHEN DIST03-NORGE                                                  
005900         MOVE 'NO ' TO BEST-IDLANDX3                                      
006000       WHEN DIST03-DANMARK                                                
006100         MOVE 'DK ' TO BEST-IDLANDX3                                      
006200       WHEN DIST03-FINLAND                                                
006300         MOVE 'FI ' TO BEST-IDLANDX3                                      
006400       WHEN W-IDDISTR = 1110 OR 1129                                      
006500         MOVE 'IS '   TO BEST-IDLANDX3                                    
006600       WHEN W-IDDISTR-2 = 12                                              
006700         MOVE 'BE '   TO BEST-IDLANDX3                                    
006800       WHEN W-IDDISTR-2 = 13                                              
006900         MOVE 'GB '   TO BEST-IDLANDX3                                    
007000       WHEN W-IDDISTR-2 = 14                                              
007100         MOVE 'FR '   TO BEST-IDLANDX3                                    
007200       WHEN W-IDDISTR-2 = 15                                              
007300         MOVE 'GR '   TO BEST-IDLANDX3                                    
007400       WHEN W-IDDISTR-2 = 16                                              
007500         MOVE 'NL '   TO BEST-IDLANDX3                                    
007600       WHEN W-IDDISTR-2 = 17                                              
007700         MOVE 'IE '   TO BEST-IDLANDX3                                    
007800       WHEN W-IDDISTR-2 = 18                                              
007900         MOVE 'IT '   TO BEST-IDLANDX3                                    
008000       WHEN W-IDDISTR-2 = 19                                              
008100         MOVE 'PT '   TO BEST-IDLANDX3                                    
008200       WHEN W-IDDISTR-3 = 192 OR 193                                      
008300         MOVE 'PT '   TO BEST-IDLANDX3                                    
008400       WHEN W-IDDISTR-2 = 20                                              
008500         MOVE 'CH '   TO BEST-IDLANDX3                                    
008600       WHEN W-IDDISTR-2 = 21                                              
008700         MOVE 'ES '   TO BEST-IDLANDX3                                    
008800       WHEN W-IDDISTR-2 = 22                                              
008900         MOVE 'DE '   TO BEST-IDLANDX3                                    
009000       WHEN W-IDDISTR-2 = 23                                              
009100         MOVE 'AT '   TO BEST-IDLANDX3                                    
009200       WHEN W-IDDISTR-2 = 24                                              
009300         MOVE 'SI '   TO BEST-IDLANDX3                                    
009400       WHEN W-IDDISTR-3 = 254                                             
009500         MOVE 'BG '   TO BEST-IDLANDX3                                    
009600       WHEN W-IDDISTR-3 = 255                                             
009700         MOVE 'HU '   TO BEST-IDLANDX3                                    
009800       WHEN W-IDDISTR-3 = 256                                             
009900         MOVE 'BG '   TO BEST-IDLANDX3                                    
010000       WHEN W-IDDISTR-3 = 257                                             
010100         MOVE 'BG '   TO BEST-IDLANDX3                                    
010200       WHEN W-IDDISTR   = 2602 OR 2606 OR 2607 OR 2610 OR 2613 OR         
010300                          2619 OR 2622 OR 2623 OR 2624 OR 2627 OR         
010400                          2628 OR 2632 OR 2633 OR 2640 OR 2647 OR         
010500                          2648 OR 2668 OR 2698                            
010600         MOVE 'RU '   TO BEST-IDLANDX3                                    
010700       WHEN W-IDDISTR   = 2612 OR 2645 OR 2656                            
010800         MOVE 'LV '   TO BEST-IDLANDX3                                    
010900       WHEN W-IDDISTR   = 2615 OR 2617 OR 2618 OR 2629 OR 2631 OR         
011000                          2661 OR 2662                                    
011100         MOVE 'UA '   TO BEST-IDLANDX3                                    
011200       WHEN W-IDDISTR   = 2620 OR 2643                                    
011300         MOVE 'BY '   TO BEST-IDLANDX3                                    
011400       WHEN W-IDDISTR   = 2630 OR 2653                                    
011500         MOVE 'EE '   TO BEST-IDLANDX3                                    
011600       WHEN W-IDDISTR   = 2635 OR 2650 OR 2652                            
011700         MOVE 'LT '   TO BEST-IDLANDX3                                    
011800       WHEN W-IDDISTR   = 2644 OR 2649 OR 2655                            
011900         MOVE 'KZ '   TO BEST-IDLANDX3                                    
012000       WHEN W-IDDISTR   = 2621 OR 2623 OR 2681                            
012100         MOVE 'MD '   TO BEST-IDLANDX3                                    
012200       WHEN W-IDDISTR   = 2626 OR 2634                                    
012300         MOVE 'UZ '   TO BEST-IDLANDX3                                    
012400       WHEN W-IDDISTR   = 2665                                            
012500         MOVE 'AZ '   TO BEST-IDLANDX3                                    
012600       WHEN W-IDDISTR   = 2677 OR 2688                                    
012700         MOVE 'GE '   TO BEST-IDLANDX3                                    
012800       WHEN W-IDDISTR   = 2678                                            
012900         MOVE 'TM '   TO BEST-IDLANDX3                                    
013000       WHEN W-IDDISTR   = 2679                                            
013100         MOVE 'EE '   TO BEST-IDLANDX3                                    
013200       WHEN W-IDDISTR   = 2696                                            
013300         MOVE 'AM '   TO BEST-IDLANDX3                                    
013400       WHEN W-IDDISTR-2 = 26                                              
013500         MOVE 'RU '   TO BEST-IDLANDX3                                    
013600       WHEN W-IDDISTR   = 2715                                            
013700         MOVE 'UA '   TO BEST-IDLANDX3                                    
013800       WHEN W-IDDISTR-3 = 274                                             
013900         MOVE 'XC '   TO BEST-IDLANDX3                                    
014000       WHEN W-IDDISTR-3 = 275 OR 276                                      
014100         MOVE 'DD '   TO BEST-IDLANDX3                                    
014200       WHEN W-IDDISTR-3 = 285                                             
014300         MOVE 'RO '   TO BEST-IDLANDX3                                    
014400       WHEN W-IDDISTR-3 = 286                                             
014500         MOVE 'RO '   TO BEST-IDLANDX3                                    
014600       WHEN W-IDDISTR   = 2878                                            
014700         MOVE 'PL '   TO BEST-IDLANDX3                                    
014800       WHEN W-IDDISTR-2 = 28                                              
014900         MOVE 'PL '   TO BEST-IDLANDX3                                    
015000       WHEN W-IDDISTR   = 3001                                            
015100         MOVE 'KE '   TO BEST-IDLANDX3                                    
015200       WHEN W-IDDISTR-3 = 300                                             
015300         MOVE 'AL '   TO BEST-IDLANDX3                                    
015400       WHEN W-IDDISTR-3 = 301                                             
015500         MOVE 'GI '   TO BEST-IDLANDX3                                    
015600       WHEN W-IDDISTR-3 = 302                                             
015700         MOVE 'MT '   TO BEST-IDLANDX3                                    
015800       WHEN W-IDDISTR   = 3130                                            
015900         MOVE 'AO '   TO BEST-IDLANDX3                                    
016000       WHEN W-IDDISTR   = 3160                                            
016100         MOVE 'ZA '   TO BEST-IDLANDX3                                    
016200       WHEN W-IDDISTR   = 3161                                            
016300         MOVE 'ZA '   TO BEST-IDLANDX3                                    
016400       WHEN W-IDDISTR   = 3162                                            
016500         MOVE 'ZA '   TO BEST-IDLANDX3                                    
016600       WHEN W-IDDISTR   = 3163                                            
016700         MOVE 'ZA '   TO BEST-IDLANDX3                                    
016800       WHEN W-IDDISTR   = 3166                                            
016900         MOVE 'ZA '   TO BEST-IDLANDX3                                    
017000       WHEN W-IDDISTR-2 = 32                                              
017100         MOVE 'EG '   TO BEST-IDLANDX3                                    
017200       WHEN W-IDDISTR-2 = 33                                              
017300         MOVE 'GH '   TO BEST-IDLANDX3                                    
017400       WHEN W-IDDISTR-2 = 34                                              
017500         MOVE 'ZR '   TO BEST-IDLANDX3                                    
017600       WHEN W-IDDISTR-2 = 35                                              
017700         MOVE 'LR '   TO BEST-IDLANDX3                                    
017800       WHEN W-IDDISTR-2 = 36                                              
017900         MOVE 'MA '   TO BEST-IDLANDX3                                    
018000       WHEN W-IDDISTR   = 3743                                            
018100         MOVE 'ZW '   TO BEST-IDLANDX3                                    
018200       WHEN W-IDDISTR-2 = 37                                              
018300         MOVE 'MZ '   TO BEST-IDLANDX3                                    
018400       WHEN W-IDDISTR-2 = 38                                              
018500         MOVE 'NG '   TO BEST-IDLANDX3                                    
018600       WHEN W-IDDISTR-2 = 39                                              
018700         MOVE 'ZW '   TO BEST-IDLANDX3                                    
018800       WHEN W-IDDISTR-2 = 40                                              
018900         MOVE 'ZA '   TO BEST-IDLANDX3                                    
019000       WHEN W-IDDISTR-2 = 41                                              
019100         MOVE 'KE '   TO BEST-IDLANDX3                                    
019200       WHEN W-IDDISTR-2 = 42                                              
019300         MOVE 'UG '   TO BEST-IDLANDX3                                    
019400       WHEN W-IDDISTR-3 = 431 OR 432 OR 433 OR 434                        
019500         MOVE 'ZM '   TO BEST-IDLANDX3                                    
019600       WHEN W-IDDISTR-3 = 436 OR 437                                      
019700         MOVE 'DZ '   TO BEST-IDLANDX3                                    
019800       WHEN W-IDDISTR   = 4400 OR 4410                                    
019900         MOVE 'TN '   TO BEST-IDLANDX3                                    
020000       WHEN W-IDDISTR   = 4401                                            
020100         MOVE 'DZ '   TO BEST-IDLANDX3                                    
020200       WHEN W-IDDISTR   = 4412                                            
020300         MOVE 'LY '   TO BEST-IDLANDX3                                    
020310       WHEN W-IDDISTR   = 4553                                            
020320         MOVE 'PY '   TO BEST-IDLANDX3                                    
020400       WHEN W-IDDISTR   = 4560 OR 4570                                    
020500         MOVE 'GH '   TO BEST-IDLANDX3                                    
020710       WHEN W-IDDISTR-2 = 45                                              
020720         MOVE 'TZ '   TO BEST-IDLANDX3                                    
020800       WHEN W-IDDISTR-3 = 460 OR 461 OR 462                               
020900         MOVE 'ET '   TO BEST-IDLANDX3                                    
021000       WHEN W-IDDISTR-3 = 463                                             
021100         MOVE 'SD '   TO BEST-IDLANDX3                                    
021200       WHEN W-IDDISTR-3 = 480                                             
021300         MOVE 'ET '   TO BEST-IDLANDX3                                    
021400       WHEN W-IDDISTR   = 4810                                            
021500         MOVE 'MU '   TO BEST-IDLANDX3                                    
021510       WHEN W-IDDISTR   = 4811                                            
021520         MOVE 'AO '   TO BEST-IDLANDX3                                    
021600       WHEN W-IDDISTR-3 = 481                                             
021700         MOVE 'SA '   TO BEST-IDLANDX3                                    
021800       WHEN W-IDDISTR-3 = 482                                             
021900         MOVE 'SD '   TO BEST-IDLANDX3                                    
022000       WHEN W-IDDISTR-3 = 483                                             
022100         MOVE 'LY '   TO BEST-IDLANDX3                                    
022200       WHEN W-IDDISTR   = 4849                                            
022300         MOVE 'LY '   TO BEST-IDLANDX3                                    
022400       WHEN W-IDDISTR-3 = 484                                             
022500         MOVE 'SA '   TO BEST-IDLANDX3                                    
022600       WHEN W-IDDISTR   = 4850 OR 4860                                    
022700         MOVE 'SA '   TO BEST-IDLANDX3                                    
022800       WHEN W-IDDISTR-3 = 485                                             
022900         MOVE 'TN '   TO BEST-IDLANDX3                                    
023000       WHEN W-IDDISTR-3 = 488                                             
023100         MOVE 'ES '   TO BEST-IDLANDX3                                    
023200       WHEN W-IDDISTR-2 = 49                                              
023300         MOVE 'IQ '   TO BEST-IDLANDX3                                    
023400       WHEN W-IDDISTR-2 = 50                                              
023500         MOVE 'IR '   TO BEST-IDLANDX3                                    
023600       WHEN W-IDDISTR-2 = 51                                              
023700         MOVE 'IL '   TO BEST-IDLANDX3                                    
023800       WHEN W-IDDISTR   = 5210 OR 5211 OR 5214                            
023900         MOVE 'VN '   TO BEST-IDLANDX3                                    
024000       WHEN W-IDDISTR   = 5216                                            
024100         MOVE 'KH '   TO BEST-IDLANDX3                                    
024200       WHEN W-IDDISTR-2 = 52                                              
024300         MOVE 'JP '   TO BEST-IDLANDX3                                    
024400       WHEN W-IDDISTR-2 = 53                                              
024500         MOVE 'JO '   TO BEST-IDLANDX3                                    
024600       WHEN W-IDDISTR   = 5415                                            
024700         MOVE 'QA '   TO BEST-IDLANDX3                                    
024800       WHEN W-IDDISTR-2 = 54                                              
024900         MOVE 'KW '   TO BEST-IDLANDX3                                    
025000       WHEN W-IDDISTR-2 = 55                                              
025100         MOVE 'LB '   TO BEST-IDLANDX3                                    
025200       WHEN W-IDDISTR = 5616                                              
025300         MOVE 'SG '   TO BEST-IDLANDX3                                    
025400       WHEN W-IDDISTR = 5620                                              
025500         MOVE 'MM '   TO BEST-IDLANDX3                                    
025600       WHEN W-IDDISTR = 5624 OR 5625                                      
025700         MOVE 'BN '   TO BEST-IDLANDX3                                    
025800       WHEN W-IDDISTR-2 = 56                                              
025900         MOVE 'MY '   TO BEST-IDLANDX3                                    
026000       WHEN W-IDDISTR-2 = 57                                              
026100         MOVE 'SY '   TO BEST-IDLANDX3                                    
026200       WHEN W-IDDISTR   = 5840                                            
026300         MOVE 'CY '   TO BEST-IDLANDX3                                    
026400       WHEN W-IDDISTR-2 = 58                                              
026500         MOVE 'TR '   TO BEST-IDLANDX3                                    
026600       WHEN W-IDDISTR = 5922                                              
026700         MOVE 'FJ '   TO BEST-IDLANDX3                                    
026800       WHEN W-IDDISTR-2 = 59                                              
026900         MOVE 'ID '   TO BEST-IDLANDX3                                    
027000       WHEN W-IDDISTR-3 = 600                                             
027100         MOVE 'LA '   TO BEST-IDLANDX3                                    
027200       WHEN W-IDDISTR   = 6010                                            
027300         MOVE 'IN '   TO BEST-IDLANDX3                                    
027400       WHEN W-IDDISTR-3 = 601                                             
027500         MOVE 'PH '   TO BEST-IDLANDX3                                    
027600       WHEN W-IDDISTR   = 6021                                            
027700         MOVE 'GU '   TO BEST-IDLANDX3                                    
027800       WHEN W-IDDISTR   = 6026                                            
027900         MOVE 'BD '   TO BEST-IDLANDX3                                    
028000       WHEN W-IDDISTR   = 6028                                            
028100         MOVE 'LK '   TO BEST-IDLANDX3                                    
028200       WHEN W-IDDISTR   = 6029                                            
028300         MOVE 'IN '   TO BEST-IDLANDX3                                    
028400       WHEN W-IDDISTR-3 = 602                                             
028500         MOVE 'BD '   TO BEST-IDLANDX3                                    
028600       WHEN W-IDDISTR-3 = 603                                             
028700         MOVE 'IN '   TO BEST-IDLANDX3                                    
028800       WHEN W-IDDISTR-3 = 604                                             
028900         MOVE 'IN '   TO BEST-IDLANDX3                                    
029000       WHEN W-IDDISTR   = 6050 OR 6051 OR 6052 OR 6054 OR                 
029100                          6055 OR 6056 OR 6057 OR 6058 OR                 
029200                          6059                                            
029300         MOVE 'IN '   TO BEST-IDLANDX3                                    
029400       WHEN W-IDDISTR   = 6053                                            
029500         MOVE 'PK '   TO BEST-IDLANDX3                                    
029600       WHEN W-IDDISTR   = 6080                                            
029700         MOVE 'IN '   TO BEST-IDLANDX3                                    
029800       WHEN W-IDDISTR-2 = 61                                              
029900         MOVE 'KR '   TO BEST-IDLANDX3                                    
030000       WHEN W-IDDISTR   = 6200                                            
030100         MOVE 'TW '   TO BEST-IDLANDX3                                    
030200       WHEN W-IDDISTR   = 6201                                            
030300         MOVE 'CN '   TO BEST-IDLANDX3                                    
030400       WHEN W-IDDISTR   = 6202                                            
030500         MOVE 'HK '   TO BEST-IDLANDX3                                    
030600       WHEN W-IDDISTR   = 6203 OR 6207                                    
030700         MOVE 'TW '   TO BEST-IDLANDX3                                    
030800       WHEN W-IDDISTR-3 = 6204 OR 6205 OR 6206 OR                         
030900                          6208                                            
031000         MOVE 'CN '   TO BEST-IDLANDX3                                    
031100       WHEN W-IDDISTR   = 6209                                            
031200         MOVE 'LK '   TO BEST-IDLANDX3                                    
031300       WHEN W-IDDISTR = 6210                                              
031400         MOVE 'CY '   TO BEST-IDLANDX3                                    
031500       WHEN W-IDDISTR = 6214                                              
031600         MOVE 'HK '   TO BEST-IDLANDX3                                    
031700       WHEN W-IDDISTR = 6240                                              
031800         MOVE 'HK '   TO BEST-IDLANDX3                                    
031900       WHEN W-IDDISTR = 6222                                              
032000         MOVE 'TW '   TO BEST-IDLANDX3                                    
032100       WHEN W-IDDISTR = 6230 OR 6231                                      
032200         MOVE 'YE '   TO BEST-IDLANDX3                                    
032300       WHEN W-IDDISTR = 6234 OR 6233                                      
032400         MOVE 'OM '   TO BEST-IDLANDX3                                    
032500       WHEN W-IDDISTR = 6236                                              
032600         MOVE 'BH '   TO BEST-IDLANDX3                                    
032700       WHEN W-IDDISTR = 6239                                              
032800         MOVE 'OM '   TO BEST-IDLANDX3                                    
032900       WHEN W-IDDISTR = 6241                                              
033000         MOVE 'AE '   TO BEST-IDLANDX3                                    
033100       WHEN W-IDDISTR = 6244                                              
033200         MOVE 'AE '   TO BEST-IDLANDX3                                    
033300       WHEN W-IDDISTR = 6247                                              
033400         MOVE 'AE '   TO BEST-IDLANDX3                                    
033500       WHEN W-IDDISTR = 6225 OR 6251                                      
033600         MOVE 'TH '   TO BEST-IDLANDX3                                    
033700       WHEN W-IDDISTR = 6271 OR 6274                                      
033800         MOVE 'CN '   TO BEST-IDLANDX3                                    
033900       WHEN W-IDDISTR-2 = 63                                              
034000         MOVE 'AR '   TO BEST-IDLANDX3                                    
034100       WHEN W-IDDISTR-2 = 64                                              
034200         MOVE 'CL '   TO BEST-IDLANDX3                                    
034300       WHEN W-IDDISTR-2 = 65                                              
034400         MOVE 'MX '   TO BEST-IDLANDX3                                    
034700       WHEN W-IDDISTR-2 = 67                                              
034800         MOVE 'PE '   TO BEST-IDLANDX3                                    
034900       WHEN W-IDDISTR-2 = 68                                              
035000         MOVE 'UY '   TO BEST-IDLANDX3                                    
035100       WHEN W-IDDISTR-2 = 69                                              
035200         MOVE 'VE '   TO BEST-IDLANDX3                                    
035300       WHEN W-IDDISTR = 7080                                              
035400         MOVE 'SR '   TO BEST-IDLANDX3                                    
035500       WHEN W-IDDISTR-2 = 70                                              
035600         MOVE 'BR '   TO BEST-IDLANDX3                                    
035700       WHEN W-IDDISTR = 7140                                              
035800         MOVE 'BR '   TO BEST-IDLANDX3                                    
035900       WHEN W-IDDISTR = 7190                                              
036000         MOVE 'CR '   TO BEST-IDLANDX3                                    
036100       WHEN W-IDDISTR = 7290                                              
036200         MOVE 'PA '   TO BEST-IDLANDX3                                    
036300       WHEN W-IDDISTR = 7390                                              
036400         MOVE 'HN '   TO BEST-IDLANDX3                                    
036500       WHEN W-IDDISTR-3 = 740                                             
036600         MOVE 'EC '   TO BEST-IDLANDX3                                    
036700       WHEN W-IDDISTR = 7426                                              
036800         MOVE 'PF '   TO BEST-IDLANDX3                                    
036900       WHEN W-IDDISTR = 7450                                              
037000         MOVE 'TT '   TO BEST-IDLANDX3                                    
037100       WHEN W-IDDISTR = 7472                                              
037200         MOVE 'DO '   TO BEST-IDLANDX3                                    
037301       WHEN W-IDDISTR = 7474                                              
037400         MOVE 'PR '   TO BEST-IDLANDX3                                    
037500       WHEN W-IDDISTR-3 = 747                                             
037600         MOVE 'PA '   TO BEST-IDLANDX3                                    
037900       WHEN W-IDDISTR = 7481 OR 7482                                      
038000         MOVE 'CO '   TO BEST-IDLANDX3                                    
038100       WHEN W-IDDISTR = 7490                                              
038200         MOVE 'GT '   TO BEST-IDLANDX3                                    
038300       WHEN W-IDDISTR = 7496                                              
038400         MOVE 'DO '   TO BEST-IDLANDX3                                    
038500       WHEN W-IDDISTR-2 = 74                                              
038600         MOVE 'EC '   TO BEST-IDLANDX3                                    
038700       WHEN W-IDDISTR-3 = 750 OR 751 OR 752 OR 753 OR 754 OR              
038800                          755 OR 756 OR 757                               
038900         MOVE 'US '   TO BEST-IDLANDX3                                    
039000       WHEN W-IDDISTR   = 7590                                            
039100         MOVE 'SV '   TO BEST-IDLANDX3                                    
039200       WHEN W-IDDISTR = 7656                                              
039200         MOVE 'EC '   TO BEST-IDLANDX3                                    
039200       WHEN W-IDDISTR = 7690                                              
039300         MOVE 'BO '   TO BEST-IDLANDX3                                    
039400       WHEN W-IDDISTR-2 = 76                                              
039500         MOVE 'CA '   TO BEST-IDLANDX3                                    
039600       WHEN W-IDDISTR   = 7701                                            
039700         MOVE 'GB '   TO BEST-IDLANDX3                                    
039800       WHEN W-IDDISTR = 7870 OR 7871                                      
039900         MOVE 'NC '   TO BEST-IDLANDX3                                    
             WHEN W-IDDISTR   = 7899                                            
               MOVE 'UY '   TO BEST-IDLANDX3                                    
040000       WHEN W-IDDISTR-2 = 78                                              
040100         MOVE 'AU '   TO BEST-IDLANDX3                                    
040200       WHEN W-IDDISTR-2 = 79                                              
040300         MOVE 'NZ '   TO BEST-IDLANDX3                                    
040400       WHEN W-IDDISTR = 8031                                              
040500         MOVE 'NL '   TO BEST-IDLANDX3                                    
040600       WHEN W-IDDISTR = 8092                                              
040700         MOVE 'GB '   TO BEST-IDLANDX3                                    
040800       WHEN W-IDDISTR-2 = 80                                              
040900         MOVE 'BE '   TO BEST-IDLANDX3                                    
041000       WHEN W-IDDISTR   = 8111                                            
041100         MOVE 'SE '   TO BEST-IDLANDX3                                    
041200       WHEN W-IDDISTR-3 = 814                                             
041300         MOVE 'US '   TO BEST-IDLANDX3                                    
041400       WHEN W-IDDISTR   = 8152                                            
041500         MOVE 'BR '   TO BEST-IDLANDX3                                    
041600       WHEN W-IDDISTR   = 8153                                            
041700         MOVE 'MX '   TO BEST-IDLANDX3                                    
041800       WHEN W-IDDISTR-3 = 815                                             
041900         MOVE 'CA '   TO BEST-IDLANDX3                                    
042000       WHEN W-IDDISTR   = 8161                                            
042100         MOVE 'JP '   TO BEST-IDLANDX3                                    
042200       WHEN W-IDDISTR   = 8162                                            
042300         MOVE 'AU '   TO BEST-IDLANDX3                                    
042400       WHEN W-IDDISTR   = 8163                                            
042500         MOVE 'TH '   TO BEST-IDLANDX3                                    
042600       WHEN W-IDDISTR   = 8164                                            
042700         MOVE 'TW '   TO BEST-IDLANDX3                                    
042800       WHEN W-IDDISTR   = 8165                                            
042900         MOVE 'KR '   TO BEST-IDLANDX3                                    
043000       WHEN W-IDDISTR   = 8166                                            
043100         MOVE 'MY '   TO BEST-IDLANDX3                                    
043200       WHEN W-IDDISTR   = 8167                                            
043300         MOVE 'IN '   TO BEST-IDLANDX3                                    
043400       WHEN W-IDDISTR-3 = 817                                             
043500         MOVE 'CN '   TO BEST-IDLANDX3                                    
043600       WHEN W-IDDISTR   = 8181                                            
043700         MOVE 'RU '   TO BEST-IDLANDX3                                    
043800       WHEN W-IDDISTR   = 8185                                            
043900         MOVE 'ZA '   TO BEST-IDLANDX3                                    
044000       WHEN W-IDDISTR   = 8186                                            
044100         MOVE 'TR '   TO BEST-IDLANDX3                                    
044200       WHEN W-IDDISTR   = 8187                                            
044300         MOVE 'AE '   TO BEST-IDLANDX3                                    
044400       WHEN W-IDDISTR   = 8211                                            
044500         MOVE 'SE '   TO BEST-IDLANDX3                                    
044600       WHEN W-IDDISTR   = 8500                                            
044700         MOVE 'JP '   TO BEST-IDLANDX3                                    
044800       WHEN W-IDDISTR   = 8802 OR 8807                                    
044900         MOVE 'GB '   TO BEST-IDLANDX3                                    
045000       WHEN W-IDDISTR   = 8851                                            
045100         MOVE 'GB '   TO BEST-IDLANDX3                                    
045200       WHEN W-IDDISTR   = 8857                                            
045300         MOVE 'CH '   TO BEST-IDLANDX3                                    
045400       WHEN W-IDDISTR   = 8859                                            
045500         MOVE 'NO '   TO BEST-IDLANDX3                                    
045600       WHEN W-IDDISTR   = 9111                                            
045700         MOVE 'SE '   TO BEST-IDLANDX3                                    
045800       WHEN W-IDDISTR   = 9141 OR 9142 OR 9143 OR                         
045900                          9144 OR 9145 OR 9146 OR                         
046000                          9147 OR 9148 OR 9149                            
046100         MOVE 'US '   TO BEST-IDLANDX3                                    
046200       WHEN W-IDDISTR   = 9153                                            
046300         MOVE 'MX '   TO BEST-IDLANDX3                                    
046310       WHEN W-IDDISTR   = 9161                                            
046320         MOVE 'JP '   TO BEST-IDLANDX3                                    
046400       WHEN W-IDDISTR   = 9162                                            
046500         MOVE 'AU '   TO BEST-IDLANDX3                                    
046600       WHEN W-IDDISTR   = 9163                                            
046700         MOVE 'TH '   TO BEST-IDLANDX3                                    
046800       WHEN W-IDDISTR   = 9164                                            
046900         MOVE 'TW '   TO BEST-IDLANDX3                                    
047000       WHEN W-IDDISTR   = 9165                                            
047100         MOVE 'KR '   TO BEST-IDLANDX3                                    
047200       WHEN W-IDDISTR   = 9166                                            
047300         MOVE 'MY '   TO BEST-IDLANDX3                                    
047400       WHEN W-IDDISTR   = 9167                                            
047500         MOVE 'IN '   TO BEST-IDLANDX3                                    
047600       WHEN W-IDDISTR   = 9211                                            
047700         MOVE 'SE '   TO BEST-IDLANDX3                                    
047800       WHEN W-IDDISTR   = 9271                                            
047900         MOVE 'CN '   TO BEST-IDLANDX3                                    
048000       WHEN OTHER                                                         
048100         MOVE SPACE   TO BEST-IDLANDX3                                    
048200                                                                          
048300     END-EVALUATE                                                         
048400     .                                                                    
048500     EJECT                                                                
