000010*** EDIT ALLOWED                                                          
000011*                            *************************************        
000012*                            *** ANVÄNDS VID TEST AV:                     
000013*                            ***  - VILKET LAND TILLHÖR ETT               
000014*                            ***    IDDC (BARA NDC).                      
000015*                            ***                                          
000016*                            ***    DC 40 HÄMTAR LAND KOD FÖR USA         
000017*                            ***    DC 70 HÄMTAR LAND KOD FÖR KINA        
000018*                            ***                                          
000019*                            ***    ANVÄND SEARCH ALL FÖR ATT             
000020*                            ***    SÖKA I TABELLEN.                      
000021*                            ***                                          
000022*                            ***  OBS!!!                                  
000023*                            *** 1. TABELLEN MÅSTE VARA SORTERAD          
000024*                            ***    I DCLAND-IDDC ORDNING.                
000025*                            *** 2. OM MAN LÄGGER TILL NYA IDDC           
000026*                            ***    SE TILL ATT ANPASSA OCCURS            
000027*                            ***    SAMT DCLAND-IX-MAX                    
000028*                            *************************************        
000029*                                                                         
000030 01  DCLAND-TABELL-VALUES.                                                
000040     03  FILLER             PIC X(5) VALUE '11 SE'.                       
000050     03  FILLER             PIC X(5) VALUE '12 NL'.                       
000060     03  FILLER             PIC X(5) VALUE '40 US'.                       
000070     03  FILLER             PIC X(5) VALUE '41 US'.                       
000080     03  FILLER             PIC X(5) VALUE '43 US'.                       
000090     03  FILLER             PIC X(5) VALUE '44 US'.                       
000100     03  FILLER             PIC X(5) VALUE '45 US'.                       
000200     03  FILLER             PIC X(5) VALUE '46 US'.                       
000210     03  FILLER             PIC X(5) VALUE '47 US'.                       
000300     03  FILLER             PIC X(5) VALUE '51 CA'.                       
000400     03  FILLER             PIC X(5) VALUE '52 BR'.                       
000500     03  FILLER             PIC X(5) VALUE '53 MX'.                       
000501     03  FILLER             PIC X(5) VALUE '6A JP'.                       
000502     03  FILLER             PIC X(5) VALUE '61 JP'.                       
000503     03  FILLER             PIC X(5) VALUE '62 AU'.                       
000504     03  FILLER             PIC X(5) VALUE '63 TH'.                       
000505     03  FILLER             PIC X(5) VALUE '64 TW'.                       
000506     03  FILLER             PIC X(5) VALUE '65 KR'.                       
000507     03  FILLER             PIC X(5) VALUE '66 MY'.                       
000508     03  FILLER             PIC X(5) VALUE '67 IN'.                       
000509     03  FILLER             PIC X(5) VALUE '70 CN'.                       
000510     03  FILLER             PIC X(5) VALUE '71 CN'.                       
000511     03  FILLER             PIC X(5) VALUE '72 CN'.                       
000512     03  FILLER             PIC X(5) VALUE '73 CN'.                       
000513     03  FILLER             PIC X(5) VALUE '74 CN'.                       
000514     03  FILLER             PIC X(5) VALUE '81 RU'.                       
000515     03  FILLER             PIC X(5) VALUE '85 ZA'.                       
000516     03  FILLER             PIC X(5) VALUE '86 TR'.                       
000517     03  FILLER             PIC X(5) VALUE '87 AE'.                       
000518     03  FILLER             PIC X(5) VALUE '92 US'.                       
000519     03  FILLER             PIC X(5) VALUE '93 TH'.                       
000520*                                                                         
000521 01  DC-LAND-TAB REDEFINES DCLAND-TABELL-VALUES.                          
000530     03  DC-LAND OCCURS 31 TIMES                                          
000540                          ASCENDING KEY IS DCLAND-IDDC                    
000550                          INDEXED BY DCLAND-IX.                           
000560       05  DCLAND-IDDC         PIC X(2).                                  
000570       05  FILLER              PIC X(1).                                  
000580       05  DCLAND-IDLANDX2     PIC X(2).                                  
000590           88 DCLAND-ARABEMIRATES    VALUE 'AE'.                          
000600           88 DCLAND-AUSTRALIA       VALUE 'AU'.                          
000700           88 DCLAND-CANADA          VALUE 'CA'.                          
000800           88 DCLAND-CHINA           VALUE 'CN'.                          
000900           88 DCLAND-INDIA           VALUE 'IN'.                          
001000           88 DCLAND-JAPAN           VALUE 'JP'.                          
001100           88 DCLAND-KOREA           VALUE 'KR'.                          
001200           88 DCLAND-MALAYSIA        VALUE 'MY'.                          
001300           88 DCLAND-HOLLAND-TR      VALUE 'NL'.                          
001400           88 DCLAND-RUSSIA          VALUE 'RU'.                          
001500           88 DCLAND-SWEDEN          VALUE 'SE'.                          
001600           88 DCLAND-THAILAND        VALUE 'TH'.                          
001700           88 DCLAND-TAIWAN          VALUE 'TW'.                          
001800           88 DCLAND-USA             VALUE 'US'.                          
001900*                                                                         
002000 01  DCLAND-IX-MAX             PIC S9(3)                                  
002100                                     VALUE +31 COMP-3.                    
002200*                                                                         
