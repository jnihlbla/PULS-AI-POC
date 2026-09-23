001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W476LAST.                                                
001300 AUTHOR.         STINA MOGREN.                                            
001400 DATE-WRITTEN.   04/11/19.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        SUBPROGRAM TO WRITE 'LAST INFO          ' TRANSPORT              
001900*        DOCUMENT. IT IS CALLED BY A PROGRAM W40674. DOCUMENT SHOW        
002000*        EACH DEALER,ADDRESS AND EACH KOLLI. FOR EACH KOLLI IT            
002100*        SHOWS PACKAGE SIZE,WEIGHT,VOLUME AND IS THAT CONTAINS            
002200*        HAZARDOUS GOODS. AFTER EACH DEALER IT SHOWS TOTAL LINE           
002300*        FOR TOTAL KOLLI,WEIGHT,VOLUME,VALUE.AT THE END OF THE            
002400*        DOCUMENT IT SHOWS THE TOTAL FOR THE FULL SHIPMENT/DIST           
002500*                                                                         
002601*        THE PROGRAM READS     WDE1                                       
002602*        THE PROGRAM READS     WDE6                                       
002610*        THE PROGRAM READS     WDR1                                       
002620*                    READS     WDB9                                       
002700*                                                                         
002800*    ABENDCODES:                                                          
002900*        U0016 -  . . . .                                                 
003000*        U1000 -  . . . .                                                 
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)   VALUE 'W476LAST'.             
005000 77  YES                         PIC X      VALUE 'J'.                    
005100 77  NOO                         PIC X      VALUE 'N'.                    
005200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005201 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
005203 77  INDX-MAX                    PIC S9(4)  VALUE ZERO  COMP SYNC.        
005204 77  INDX-MAX2                   PIC S9(4)  VALUE +10   COMP SYNC.        
005205 77  TYP                         PIC S9(3)  VALUE +0    COMP SYNC.        
005210 77  FLSAMFAK-SW                 PIC X(01)  VALUE 'N'.                    
005220     88 FLSAMFAK                            VALUE 'J'.                    
005221                                                                          
005222 77  LAGG-UT-NYCKLAR-SW          PIC X(01)  VALUE 'N'.                    
005223     88 LAGG-UT-NYCKLAR                     VALUE 'J'.                    
005230                                                                          
005240 77  FLATERSTARTA                PIC X      VALUE 'N'.                    
005300 77  W-PAGE-NO                   PIC S9(3)  COMP-3 VALUE ZERO.            
005310 77  W-4739                      PIC X(4)   VALUE '4739'.                 
005320 01  W-SEKT                      PIC X(16)  VALUE SPACES.                 
005400                                                                          
005410 01  W-YYMMDD                    PIC 9(06)  VALUE ZERO.                   
005420 01  W-IDDISTR-DISP              PIC 9(5)  VALUE ZERO.                    
005450                                                                          
005451*    --- STYRTECKEN PRINTER                                               
005452 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
005453 01  WS-SKIP1                      PIC X      VALUE ' '.                  
005454 01  WS-SKIP2                      PIC X      VALUE '0'.                  
005455 01  WS-SKIP3                      PIC X      VALUE '-'.                  
005456                                                                          
005457 01  WS-TYP-IDSHIP                 PIC X(19) VALUE                        
005458                                   'LOAD INFO          '.                 
005460 01  W-LINE.                                                              
005470     03  W-LINE-COUNT            PIC 9(02)  VALUE ZERO.                   
005480     03  W-LINE-MAX              PIC 9(02)  VALUE 43.                     
005490     03  W-LINE-HEAD             PIC 9(02)  VALUE 5.                      
005491     03  W-LINE-GRTOTAL          PIC 9(02)  VALUE 2.                      
005492 01  W-BEGMRK                PIC X(90)  VALUE SPACE.                      
005493                                                                          
005500 01  TODAYS-DATE                 PIC 9(6)   VALUE ZERO.                   
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
005901*                                                                         
005902 01  W-DATUM-REDIGERING.                                                  
005903     03  W-DATUM             PIC 9(6).                                    
005904     03  W-DATUM-X           REDEFINES W-DATUM.                           
005905         05  W-DATUM-AA      PIC X(2).                                    
005906         05  W-DATUM-MM      PIC X(2).                                    
005907         05  W-DATUM-DD      PIC X(2).                                    
005910*                                                                         
005920 01  W-ADKOLLI-REDIGERING.                                                
005930     03  KLOCK-SLAG          PIC 9(8).                                    
005940     03  FILLER              REDEFINES KLOCK-SLAG.                        
005950         05  W-TIMMA         PIC 9(2).                                    
005960         05  W-MINUT         PIC 9(2).                                    
005970         05  FILLER          PIC X(4).                                    
006000     EJECT                                                                
006010 01  TEXTER.                                                              
006011     03  TEXT1-GRP.                                                       
006012        05  FILLER          PIC X(6)     VALUE '(M.FL)'.                  
006013        05  FILLER          PIC X(6)     VALUE ' ETC. '.                  
006014     03  FILLER             REDEFINES TEXT1-GRP.                          
006015        05  TEXT1           OCCURS 2                                      
006016                            PIC X(6).                                     
006017     03  TEXT2-GRP.                                                       
006018        05  FILLER          PIC X(30)    VALUE                            
006019            'LASTNINGEN ÄR FAKTURERAD'.                                   
006020        05  FILLER          PIC X(30)    VALUE                            
006021            'GEFAKT. LADINGEN        '.                                   
006022     03  FILLER             REDEFINES TEXT2-GRP.                          
006023        05  TEXT2           OCCURS 2                                      
006024                            PIC X(30).                                    
006025     03  TEXT4-GRP.                                                       
006026        05  FILLER          PIC X(18)    VALUE 'BIFOGAS'.                 
006027        05  FILLER          PIC X(18)    VALUE 'BISGEV.'.                 
006028     03  FILLER             REDEFINES TEXT4-GRP.                          
006029        05  TEXT4           OCCURS 2                                      
006030                            PIC X(18).                                    
006031     03  TEXT5-GRP.                                                       
006032         05  FILLER          PIC X(18)    VALUE 'HÄMTAS EXP'.             
006033         05  FILLER          PIC X(18)    VALUE 'AFTEHALEN '.             
006034     03  FILLER              REDEFINES TEXT5-GRP.                         
006035         05  TEXT5           OCCURS 2                                     
006036                             PIC X(18).                                   
006037     03  TEXT6-GRP.                                                       
006038         05  FILLER          PIC X(18)    VALUE                           
006039             'FINNS HOS SPEDITÖR'.                                        
006040         05  FILLER          PIC X(18)    VALUE                           
006041             'REEDS BIJ BEVRACH.'.                                        
006042     03  FILLER              REDEFINES TEXT6-GRP.                         
006043         05  TEXT6           OCCURS 2                                     
006044                             PIC X(18).                                   
006045     03  TEXT7-GRP.                                                       
006046         05  FILLER          PIC X(3)     VALUE 'JA'.                     
006047         05  FILLER          PIC X(3)     VALUE 'JA'.                     
006048     03  FILLER              REDEFINES TEXT7-GRP.                         
006049         05  TEXT7           OCCURS 2     PIC X(3).                       
006050     EJECT                                                                
006051*-  - - - - - - - - - - - - - - - - LASTNINGS LISTA                       
006052 01  LLW.                                                                 
006053     03  LLW-RADMAX          PIC S9(4)   VALUE +38   COMP-3.              
006054     03  LLW-NYSIDA          PIC S9(4)   VALUE +39   COMP-3.              
006055     03  LLW-RADANT          PIC S9(3)   VALUE ZERO  COMP-3.              
006056     03  LLW-SIDNR           PIC S9(3)   VALUE ZERO  COMP-3.              
006057     03  LLW-RAD             PIC X(132).                                  
006058     03  LLW-PRINTER         PIC X(8)    VALUE SPACE.                     
006059     03  LLW-BELISTID.                                                    
006060         05  FILLER          PIC X(3)    VALUE SPACE.                     
006061         05  LLW-BELISTID-IDLASTN                                         
006062                             PIC Z(6)9.                                   
006063*                                                                         
006064*- - - - - - - - - - - - - - - - - LISTRADER LASTNINGS LISTA              
006065*                                  VARJE RUBRIKRAD FINNS PÅ TVÅ           
006066*                                  OLIKA SPRÅK, SVENSKA OCH               
006067*                                  ENGELSKA.                              
006068 01  LISTRADER.                                                           
006069*- - - - - - - - - - - - - - - - - RUBRIKRAD 1                            
006070     03  RRAD1-SPRAKGRP.                                                  
006071       05  RRAD1-SVE.                                                     
006072         07  FILLER      PIC X(27)   VALUE SPACE.                         
006073**           'VOLVO CAR PARTS            '.                               
006074         07  FILLER      PIC X(11)   VALUE SPACE.                         
006075         07  FILLER      PIC X(27)   VALUE                                
006076             'L A S T N I N G S I N F O  '.                               
006077         07  FILLER      PIC X(20)   VALUE SPACE.                         
006078         07  FILLER      PIC X(6)    VALUE 'DATUM'.                       
006079         07  FILLER      PIC X(2)    VALUE SPACE.                         
006080         07  FILLER      PIC X       VALUE '.'.                           
006081         07  FILLER      PIC X(2)    VALUE SPACE.                         
006082         07  FILLER      PIC X       VALUE '.'.                           
006083         07  FILLER      PIC X(2)    VALUE SPACE.                         
006084         07  FILLER      PIC X(3)    VALUE SPACE.                         
006085         07  FILLER      PIC X(5)    VALUE 'SIDA '.                       
006086         07  FILLER      PIC X(3)    VALUE SPACE.                         
006087         07  FILLER      PIC X(5)    VALUE SPACE.                         
006088       05  RRAD1-ENG.                                                     
006089         07  FILLER      PIC X(27)   VALUE SPACE.                         
006090**           'VOLVO CAR PARTS            '.                               
006091         07  FILLER      PIC X(11)   VALUE SPACE.                         
006092         07  FILLER      PIC X(27)   VALUE                                
006093             'L A D I N G L I J S T    '.                                 
006094         07  FILLER      PIC X(20)   VALUE SPACE.                         
006095         07  FILLER      PIC X(6)    VALUE 'DATUM'.                       
006096         07  FILLER      PIC X(2)    VALUE SPACE.                         
006097         07  FILLER      PIC X       VALUE '.'.                           
006098         07  FILLER      PIC X(2)    VALUE SPACE.                         
006099         07  FILLER      PIC X       VALUE '.'.                           
006100         07  FILLER      PIC X(2)    VALUE SPACE.                         
006101         07  FILLER      PIC X(3)    VALUE SPACE.                         
006102         07  FILLER      PIC X(5)    VALUE ' BLZ '.                       
006103         07  FILLER      PIC X(3)    VALUE SPACE.                         
006104         07  FILLER      PIC X(5)    VALUE SPACE.                         
006105     03  FILLER          REDEFINES RRAD1-SPRAKGRP.                        
006106       05  RRAD1           OCCURS 2.                                      
006107         07  FILLER      PIC X(27).                                       
006108         07  FILLER      PIC X(11).                                       
006109         07  FILLER      PIC X(27).                                       
006110         07  FILLER      PIC X(20).                                       
006111         07  FILLER      PIC X(6).                                        
006112         07  RRAD1-DATUM-AA  PIC 9(2).                                    
006113         07  FILLER      PIC X.                                           
006114         07  RRAD1-DATUM-MM  PIC 9(2).                                    
006115         07  FILLER      PIC X.                                           
006116         07  RRAD1-DATUM-DD  PIC 9(2).                                    
006117         07  FILLER      PIC X(3).                                        
006118         07  FILLER      PIC X(5).                                        
006119         07  RRAD1-SIDA  PIC Z(2)9.                                       
006120         07  FILLER      PIC X(5).                                        
006121     EJECT                                                                
006122     03  RRAD-KLOCKA-SPRAKGRP.                                            
006123       05  RRAD-KLOCKA-SVE.                                               
006124         07  FILLER      PIC X(85)  VALUE SPACE.                          
006125         07  FILLER      PIC X(6)   VALUE 'TID   '.                       
006126         07  FILLER      PIC X(2).                                        
006127         07  FILLER      PIC X      VALUE '.'.                            
006128         07  FILLER      PIC X(2).                                        
006129       05  RRAD-KLOCKA-FLAM.                                              
006130         07  FILLER      PIC X(85)  VALUE SPACE.                          
006131         07  FILLER      PIC X(6)   VALUE 'TIJD  '.                       
006132         07  FILLER      PIC X(2).                                        
006133         07  FILLER      PIC X      VALUE '.'.                            
006134         07  FILLER      PIC X(2).                                        
006135     03  FILLER          REDEFINES RRAD-KLOCKA-SPRAKGRP.                  
006136       05  RRAD-KLOCK-SLAG     OCCURS 2.                                  
006137         07  FILLER      PIC X(91).                                       
006138         07  RRAD-KLOCK-HH  PIC 9(2).                                     
006139         07  FILLER      PIC X.                                           
006140         07  RRAD-KLOCK-MM  PIC 9(2).                                     
006141   EJECT                                                                  
006142*-  - - - - - - - - - - - - - - - - RUBRIKRAD 2                           
006143     03  RRAD2-SPRAKGRP.                                                  
006144       05  RRAD2-SVE.                                                     
006145           07  FILLER      PIC X(13)   VALUE 'SKEPPNINGSNR'.              
006146           07  FILLER      PIC X(7)    VALUE SPACE.                       
006153           07  FILLER      PIC X       VALUE SPACE.                       
006154           07  FILLER      PIC X(7)    VALUE SPACE.                       
006155           07  FILLER      PIC X(30)   VALUE SPACE.                       
006156           07  FILLER      PIC X(36)   VALUE SPACE.                       
006157       05  RRAD2-ENG.                                                     
006158           07  FILLER      PIC X(13)   VALUE '    LAADNR '.               
006159           07  FILLER      PIC X(7)    VALUE SPACE.                       
006166           07  FILLER      PIC X       VALUE SPACE.                       
006167           07  FILLER      PIC X(7)    VALUE SPACE.                       
006168           07  FILLER      PIC X(30)   VALUE SPACE.                       
006169           07  FILLER      PIC X(36)   VALUE SPACE.                       
006170     03  FILLER            REDEFINES RRAD2-SPRAKGRP.                      
006171       05  RRAD2           OCCURS 2.                                      
006172           07  FILLER      PIC X(13).                                     
006173           07  RRAD2-IDLASTN                                              
006174                           PIC Z(7).                                      
006185           07  RRAD2-FAKTTEXT                                             
006186                           PIC X(30).                                     
006187           07  FILLER      PIC X(36).                                     
006188*- - - - - - - - - - - - - - - - - RUBRIKRAD 3                            
006189     03  RRAD3-SPRAKGRP.                                                  
006190       05  RRAD3-SVE.                                                     
006191           07  FILLER      PIC X(12)   VALUE 'TRANSPORTNR'.               
006192           07  FILLER      PIC X(3)    VALUE SPACES.                      
006193*          07  FILLER      PIC X(56)   VALUE SPACE.                       
006194           07  FILLER      PIC X(2)    VALUE SPACE.                       
006195           07  FILLER      PIC X(11)   VALUE 'LASTBÄRARE '.               
006196           07  FILLER      PIC X(12)   VALUE SPACE.                       
006197           07  FILLER      PIC X(2)    VALUE SPACE.                       
006198           07  FILLER      PIC X(3)    VALUE 'FK '.                       
006199           07  FILLER      PIC X(2)    VALUE SPACE.                       
006200           07  FILLER      PIC X(2)    VALUE SPACE.                       
006201           07  FILLER      PIC X(9)    VALUE 'DISTRIKT'.                  
006202           07  FILLER      PIC X(4)    VALUE SPACE.                       
006203           07  FILLER      PIC X(2)    VALUE SPACE.                       
006204           07  FILLER      PIC X(7)    VALUE SPACE.                       
006205       05  RRAD3-ENG.                                                     
006206           07  FILLER      PIC X(12)   VALUE 'TRANSPORTNR'.               
006207           07  FILLER      PIC X(3)    VALUE SPACES.                      
006208           07  FILLER      PIC X(2)    VALUE SPACE.                       
006209*          07  FILLER      PIC X(56)   VALUE SPACE.                       
006210           07  FILLER      PIC X(11)   VALUE 'TRANSPORT: '.               
006211           07  FILLER      PIC X(12)   VALUE SPACE.                       
006212           07  FILLER      PIC X(2)    VALUE SPACE.                       
006213           07  FILLER      PIC X(3)    VALUE 'VK '.                       
006214           07  FILLER      PIC X(2)    VALUE SPACE.                       
006215           07  FILLER      PIC X(2)    VALUE SPACE.                       
006216           07  FILLER      PIC X(9)    VALUE 'DISTRIKT'.                  
006217           07  FILLER      PIC X(4)    VALUE SPACE.                       
006218           07  FILLER      PIC X(2)    VALUE SPACE.                       
006219           07  FILLER      PIC X(7)    VALUE SPACE.                       
006220     03  FILLER              REDEFINES RRAD3-SPRAKGRP.                    
006221       05  RRAD3           OCCURS 2.                                      
006222           07  FILLER      PIC X(12).                                     
006223           07  RRAD3-IDTRPTNR  PIC Z(3).                                  
006224*          07  FILLER      PIC X(56).                                     
006225           07  FILLER      PIC X(2).                                      
006226           07  FILLER      PIC X(11).                                     
006227           07  RRAD3-IDLBBET PIC X(12).                                   
006228           07  FILLER      PIC X(2).                                      
006229           07  FILLER      PIC X(3).                                      
006230           07  RRAD2-KDFRAKT                                              
006231                           PIC Z(2).                                      
006232           07  FILLER      PIC X(2).                                      
006233           07  FILLER      PIC X(9).                                      
006234           07  RRAD2-IDDISTR                                              
006235                           PIC Z(4).                                      
006236           07  FILLER      PIC X(2).                                      
006237           07  RRAD2-MFL   PIC X(7).                                      
006238     EJECT                                                                
006239*- - - - - - - - - - - - - - - - - RUBRIKRAD 4                            
006240     03  RRAD4-SPRAKGRP.                                                  
006241       05  RRAD4-SVE.                                                     
006242           07  FILLER      PIC X(71)   VALUE SPACE.                       
006243           07  FILLER      PIC X(23)   VALUE 'TYP '.                      
006244           07  FILLER      PIC X(10)   VALUE 'BETECKNING'.                
006245           07  FILLER      PIC X(11)   VALUE SPACE.                       
006246       05  RRAD4-ENG.                                                     
006247           07  FILLER      PIC X(71)   VALUE SPACE.                       
006248           07  FILLER      PIC X(23)   VALUE 'TYPE'.                      
006249           07  FILLER      PIC X(10)   VALUE 'OMSCHRIJV.'.                
006250           07  FILLER      PIC X(11)   VALUE SPACE.                       
006251     03  FILLER              REDEFINES RRAD4-SPRAKGRP.                    
006252       05  RRAD4           OCCURS 2.                                      
006253           07  FILLER      PIC X(71).                                     
006254           07  FILLER      PIC X(23).                                     
006255           07  FILLER      PIC X(10).                                     
006256           07  FILLER      PIC X(11).                                     
006257*- - - - - - - - - - - - - - - - - RUBRIKRAD 5                            
006258     03  RRAD5-SPRAKGRP.                                                  
006259       05  RRAD5-SVE.                                                     
006260           07  FILLER      PIC X(16)   VALUE 'DESTINATION'.               
006261           07  FILLER      PIC X(30)   VALUE SPACE.                       
006262           07  FILLER      PIC X(25)   VALUE SPACE.                       
006263           07  FILLER      PIC X(3)    VALUE SPACE.                       
006264           07  FILLER      PIC X       VALUE SPACE.                       
006265           07  FILLER      PIC X(15)   VALUE SPACE.                       
006266           07  FILLER      PIC X(4)    VALUE SPACE.                       
006267           07  FILLER      PIC X(12)   VALUE SPACE.                       
006268           07  FILLER      PIC X(9)    VALUE SPACE.                       
006269       05  RRAD5-ENG.                                                     
006270           07  FILLER      PIC X(16)   VALUE 'BESTEMMING '.               
006271           07  FILLER      PIC X(30)   VALUE SPACE.                       
006272           07  FILLER      PIC X(25)   VALUE SPACE.                       
006273           07  FILLER      PIC X(3)    VALUE SPACE.                       
006274           07  FILLER      PIC X       VALUE SPACE.                       
006275           07  FILLER      PIC X(15)   VALUE SPACE.                       
006276           07  FILLER      PIC X(4)    VALUE SPACE.                       
006277           07  FILLER      PIC X(12)   VALUE SPACE.                       
006278           07  FILLER      PIC X(9)    VALUE SPACE.                       
006279     03  FILLER            REDEFINES RRAD5-SPRAKGRP.                      
006280       05  RRAD5           OCCURS 2.                                      
006281           07  FILLER      PIC X(16).                                     
006282           07  RRAD5-BEROUTE                                              
006283                           PIC X(30).                                     
006284           07  FILLER      PIC X(25).                                     
006285           07  RRAD5-KDLBTYP                                              
006286                           PIC Z(3).                                      
006287           07  FILLER      PIC X.                                         
006288           07  RRAD5-BELBTYP                                              
006289                           PIC X(15).                                     
006290           07  FILLER      PIC X(4).                                      
006291           07  RRAD5-IDLBBET                                              
006292                           PIC X(12).                                     
006293           07  FILLER      PIC X(9).                                      
006294*- - - - - - - - - - - - - - - - - RUBRIKRAD 6                            
006295     03  RRAD6-SPRAKGRP.                                                  
006296       05  RRAD6-SVE.                                                     
006297           07  FILLER      PIC X(16)   VALUE 'TRANSPORTNAMN'.             
006298           07  FILLER      PIC X(15)   VALUE SPACE.                       
006299           07  FILLER      PIC X(4)    VALUE SPACE.                       
006300           07  FILLER      PIC X(13)   VALUE 'AVGÅNGSDATUM'.              
006301           07  FILLER      PIC X(2)    VALUE SPACE.                       
006302           07  FILLER      PIC X       VALUE SPACE.                       
006303           07  FILLER      PIC X(2)    VALUE SPACE.                       
006304           07  FILLER      PIC X       VALUE SPACE.                       
006305           07  FILLER      PIC X(2)    VALUE SPACE.                       
006306           07  FILLER      PIC X(15)   VALUE SPACE.                       
006307           07  FILLER      PIC X(3)    VALUE SPACE.                       
006308           07  FILLER      PIC X       VALUE SPACE.                       
006309           07  FILLER      PIC X(15)   VALUE SPACE.                       
006310           07  FILLER      PIC X(4)    VALUE SPACE.                       
006311           07  FILLER      PIC X(12)   VALUE SPACE.                       
006312           07  FILLER      PIC X(3)    VALUE SPACE.                       
006313       05  RRAD6-ENG.                                                     
006314           07  FILLER      PIC X(16)   VALUE 'TRANSPORTNAAM'.             
006315           07  FILLER      PIC X(15)   VALUE SPACE.                       
006316           07  FILLER      PIC X(4)    VALUE SPACE.                       
006317           07  FILLER      PIC X(13)   VALUE 'UITG. DATUM '.              
006318           07  FILLER      PIC X(2)    VALUE SPACE.                       
006319           07  FILLER      PIC X       VALUE SPACE.                       
006320           07  FILLER      PIC X(2)    VALUE SPACE.                       
006321           07  FILLER      PIC X       VALUE SPACE.                       
006322           07  FILLER      PIC X(2)    VALUE SPACE.                       
006323           07  FILLER      PIC X(15)   VALUE SPACE.                       
006324           07  FILLER      PIC X(3)    VALUE SPACE.                       
006325           07  FILLER      PIC X       VALUE SPACE.                       
006326           07  FILLER      PIC X(15)   VALUE SPACE.                       
006327           07  FILLER      PIC X(4)    VALUE SPACE.                       
006328           07  FILLER      PIC X(12)   VALUE SPACE.                       
006329           07  FILLER      PIC X(3)    VALUE SPACE.                       
006330     03  FILLER            REDEFINES RRAD6-SPRAKGRP.                      
006331       05  RRAD6           OCCURS 2.                                      
006332           07  FILLER      PIC X(16).                                     
006333           07  RRAD6-IDTRANSP-NAMN                                        
006334                           PIC X(15).                                     
006335           07  FILLER      PIC X(4).                                      
006336           07  FILLER      PIC X(13).                                     
006337           07  RRAD6-TIAVGANG-AA                                          
006338                           PIC 9(2).                                      
006339           07  RRAD6-PUNKT1                                               
006340                           PIC X.                                         
006341           07  RRAD6-TIAVGANG-MM                                          
006342                           PIC 9(2).                                      
006343           07  RRAD6-PUNKT2                                               
006344                           PIC X.                                         
006345           07  RRAD6-TIAVGANG-DD                                          
006346                           PIC 9(2).                                      
006347           07  FILLER      PIC X(15).                                     
006348           07  RRAD6-KDLBTYP                                              
006349                           PIC Z(3).                                      
006350           07  FILLER      PIC X.                                         
006351           07  RRAD6-BELBTYP                                              
006352                           PIC X(15).                                     
006353           07  FILLER      PIC X(4).                                      
006354           07  RRAD6-IDLBBET                                              
006355                           PIC X(12).                                     
006356           07  FILLER      PIC X(3).                                      
006357*- - - - - - - - - - - - - - - - - RUBRIKRAD 7                            
006358     03   RRAD7-SPRAKGRP.                                                 
006359       05  RRAD7-SVE.                                                     
006360           07  FILLER      PIC X(16)   VALUE 'HÄMTAS AV  '.               
006361           07  FILLER      PIC X(15)   VALUE SPACE.                       
006362           07  FILLER      PIC X(11)   VALUE SPACE.                       
006363           07  FILLER      PIC X(6)    VALUE 'DATUM'.                     
006364           07  FILLER      PIC X(2)    VALUE SPACE.                       
006365           07  FILLER      PIC X       VALUE SPACE.                       
006366           07  FILLER      PIC X(2)    VALUE SPACE.                       
006367           07  FILLER      PIC X       VALUE SPACE.                       
006368           07  FILLER      PIC X(2)    VALUE SPACE.                       
006369           07  FILLER      PIC X(15)   VALUE SPACE.                       
006370           07  FILLER      PIC X(3)    VALUE SPACE.                       
006371           07  FILLER      PIC X       VALUE SPACE.                       
006372           07  FILLER      PIC X(15)   VALUE SPACE.                       
006373           07  FILLER      PIC X(4)    VALUE SPACE.                       
006374           07  FILLER      PIC X(12)   VALUE SPACE.                       
006375           07  FILLER      PIC X(3)    VALUE SPACE.                       
006376       05  RRAD7-ENG.                                                     
006377           07  FILLER      PIC X(16)   VALUE 'AFGEH. DOOR'.               
006378           07  FILLER      PIC X(15)   VALUE SPACE.                       
006379           07  FILLER      PIC X(11)   VALUE SPACE.                       
006380           07  FILLER      PIC X(6)    VALUE 'DATUM'.                     
006381           07  FILLER      PIC X(2)    VALUE SPACE.                       
006382           07  FILLER      PIC X       VALUE SPACE.                       
006383           07  FILLER      PIC X(2)    VALUE SPACE.                       
006384           07  FILLER      PIC X       VALUE SPACE.                       
006385           07  FILLER      PIC X(2)    VALUE SPACE.                       
006386           07  FILLER      PIC X(15)   VALUE SPACE.                       
006387           07  FILLER      PIC X(3)    VALUE SPACE.                       
006388           07  FILLER      PIC X       VALUE SPACE.                       
006389           07  FILLER      PIC X(15)   VALUE SPACE.                       
006390           07  FILLER      PIC X(4)    VALUE SPACE.                       
006391           07  FILLER      PIC X(12)   VALUE SPACE.                       
006392           07  FILLER      PIC X(3)    VALUE SPACE.                       
006393     03  FILLER            REDEFINES RRAD7-SPRAKGRP.                      
006394       05  RRAD7           OCCURS 2.                                      
006395           07  FILLER      PIC X(16).                                     
006396           07  RRAD7-BETEXT-HAEMT                                         
006397                           PIC X(15).                                     
006398           07  FILLER      PIC X(11).                                     
006399           07  FILLER      PIC X(6).                                      
006400           07  RRAD7-TIHAEMT-AA                                           
006401                           PIC 9(2).                                      
006402           07  RRAD7-PUNKT1                                               
006403                           PIC X.                                         
006404           07  RRAD7-TIHAEMT-MM                                           
006405                           PIC 9(2).                                      
006406           07  RRAD7-PUNKT2                                               
006407                           PIC X.                                         
006408           07  RRAD7-TIHAEMT-DD                                           
006409                           PIC 9(2).                                      
006410           07  FILLER      PIC X(15).                                     
006411           07  RRAD7-KDLBTYP                                              
006412                           PIC Z(3).                                      
006413           07  FILLER      PIC X.                                         
006414           07  RRAD7-BELBTYP                                              
006415                           PIC X(15).                                     
006416           07  FILLER      PIC X(4).                                      
006417           07  RRAD7-IDLBBET                                              
006418                           PIC X(12).                                     
006419           07  FILLER      PIC X(3).                                      
006420*- - - - - - - - - - - - - - - - - RUBRIKRAD 8                            
006421     03  RRAD8-SPRAKGRP.                                                  
006422       05  RRAD8-SVE.                                                     
006423           07  FILLER      PIC X(16)   VALUE 'NEDKÖRES TILL'.             
006424           07  FILLER      PIC X(15)   VALUE SPACE.                       
006425           07  FILLER      PIC X(11)   VALUE SPACE.                       
006426           07  FILLER      PIC X(6)    VALUE 'DATUM'.                     
006427           07  FILLER      PIC X(2)    VALUE SPACE.                       
006428           07  FILLER      PIC X       VALUE SPACE.                       
006429           07  FILLER      PIC X(2)    VALUE SPACE.                       
006430           07  FILLER      PIC X       VALUE SPACE.                       
006431           07  FILLER      PIC X(2)    VALUE SPACE.                       
006432           07  FILLER      PIC X(15)   VALUE SPACE.                       
006433           07  FILLER      PIC X(3)    VALUE SPACE.                       
006434           07  FILLER      PIC X       VALUE SPACE.                       
006435           07  FILLER      PIC X(15)   VALUE SPACE.                       
006436           07  FILLER      PIC X(4)    VALUE SPACE.                       
006437           07  FILLER      PIC X(12)   VALUE SPACE.                       
006438           07  FILLER      PIC X(3)    VALUE SPACE.                       
006439         05  RRAD8-ENG.                                                   
006440           07  FILLER      PIC X(16)   VALUE 'GEVOERD NAAR '.             
006441           07  FILLER      PIC X(15)   VALUE SPACE.                       
006442           07  FILLER      PIC X(11)   VALUE SPACE.                       
006443           07  FILLER      PIC X(6)    VALUE 'DATUM'.                     
006444           07  FILLER      PIC X(2)    VALUE SPACE.                       
006445           07  FILLER      PIC X       VALUE SPACE.                       
006446           07  FILLER      PIC X(2)    VALUE SPACE.                       
006447           07  FILLER      PIC X       VALUE SPACE.                       
006448           07  FILLER      PIC X(2)    VALUE SPACE.                       
006449           07  FILLER      PIC X(15)   VALUE SPACE.                       
006450           07  FILLER      PIC X(3)    VALUE SPACE.                       
006451           07  FILLER      PIC X       VALUE SPACE.                       
006452           07  FILLER      PIC X(15)   VALUE SPACE.                       
006453           07  FILLER      PIC X(4)    VALUE SPACE.                       
006454           07  FILLER      PIC X(12)   VALUE SPACE.                       
006455           07  FILLER      PIC X(3)    VALUE SPACE.                       
006456     03  FILLER              REDEFINES RRAD8-SPRAKGRP.                    
006457       05  RRAD8           OCCURS 2.                                      
006458           07  FILLER      PIC X(16).                                     
006459           07  RRAD8-BETEXT-NEDK                                          
006460                           PIC X(15).                                     
006461           07  FILLER      PIC X(11).                                     
006462           07  FILLER      PIC X(6).                                      
006463           07  RRAD8-TINEDK-AA                                            
006464                           PIC 9(2).                                      
006465           07  RRAD8-PUNKT1                                               
006466                           PIC X.                                         
006467           07  RRAD8-TINEDK-MM                                            
006468                           PIC 9(2).                                      
006469           07  RRAD8-PUNKT2                                               
006470                           PIC X.                                         
006471           07  RRAD8-TINEDK-DD                                            
006472                           PIC 9(2).                                      
006473           07  FILLER      PIC X(15).                                     
006474           07  RRAD8-KDLBTYP                                              
006475                           PIC Z(3).                                      
006476           07  FILLER      PIC X.                                         
006477           07  RRAD8-BELBTYP                                              
006478                           PIC X(15).                                     
006479           07  FILLER      PIC X(4).                                      
006480           07  RRAD8-IDLBBET                                              
006481                           PIC X(12).                                     
006482           07  FILLER      PIC X(3).                                      
006483*- - - - - - - - - - - - - - - - - RUBRIKRAD 9                            
006484     03  RRAD9-SPRAKGRP.                                                  
006485       05  RRAD9-SVE.                                                     
006486           07  FILLER      PIC X(16)   VALUE 'BOKNINGSNR'.                
006487           07  FILLER      PIC X(15)   VALUE SPACE.                       
006488           07  FILLER      PIC X(40)   VALUE SPACE.                       
006489           07  FILLER      PIC X(3)    VALUE SPACE.                       
006490           07  FILLER      PIC X       VALUE SPACE.                       
006491           07  FILLER      PIC X(15)   VALUE SPACE.                       
006492           07  FILLER      PIC X(4)    VALUE SPACE.                       
006493           07  FILLER      PIC X(12)   VALUE SPACE.                       
006494           07  FILLER      PIC X(3)    VALUE SPACE.                       
006495       05  RRAD9-ENG.                                                     
006496           07  FILLER      PIC X(16)   VALUE 'BOEKINGSNR'.                
006497           07  FILLER      PIC X(15)   VALUE SPACE.                       
006498           07  FILLER      PIC X(40)   VALUE SPACE.                       
006499           07  FILLER      PIC X(3)    VALUE SPACE.                       
006500           07  FILLER      PIC X       VALUE SPACE.                       
006501           07  FILLER      PIC X(15)   VALUE SPACE.                       
006502           07  FILLER      PIC X(4)    VALUE SPACE.                       
006503           07  FILLER      PIC X(12)   VALUE SPACE.                       
006504           07  FILLER      PIC X(3)    VALUE SPACE.                       
006505     03  FILLER            REDEFINES RRAD9-SPRAKGRP.                      
006506       05  RRAD9           OCCURS 2.                                      
006507           07  FILLER      PIC X(16).                                     
006508           07  RRAD9-IDBOKN                                               
006509                           PIC X(15).                                     
006510           07  FILLER      PIC X(40).                                     
006511           07  RRAD9-KDLBTYP                                              
006512                           PIC Z(3).                                      
006513           07  FILLER      PIC X.                                         
006514           07  RRAD9-BELBTYP                                              
006515                           PIC X(15).                                     
006516           07  FILLER      PIC X(4).                                      
006517           07  RRAD9-IDLBBET                                              
006518                           PIC X(12).                                     
006519           07  FILLER  PIC X(3).                                          
006520*- - - - - - - - - - - - - - - - - RUBRIKRAD 10                           
006521     03  RRAD10-SPRAKGRP.                                                 
006522       05  RRAD10-SVE.                                                    
006523           07  FILLER      PIC X(16)   VALUE 'TRANSPORTDOK.'.             
006524           07  FILLER      PIC X(18)   VALUE SPACE.                       
006525           07  FILLER      PIC X(37)   VALUE SPACE.                       
006526           07  FILLER      PIC X(3)    VALUE SPACE.                       
006527           07  FILLER      PIC X       VALUE SPACE.                       
006528           07  FILLER      PIC X(15)   VALUE SPACE.                       
006529           07  FILLER      PIC X(4)    VALUE SPACE.                       
006530           07  FILLER      PIC X(12)   VALUE SPACE.                       
006531           07  FILLER      PIC X(3)    VALUE SPACE.                       
006532       05  RRAD10-ENG.                                                    
006533           07  FILLER      PIC X(16)   VALUE 'TRANSPORTDOK.'.             
006534           07  FILLER      PIC X(18)   VALUE SPACE.                       
006535           07  FILLER      PIC X(37)   VALUE SPACE.                       
006536           07  FILLER      PIC X(3)    VALUE SPACE.                       
006537           07  FILLER      PIC X       VALUE SPACE.                       
006538           07  FILLER      PIC X(15)   VALUE SPACE.                       
006539           07  FILLER      PIC X(4)    VALUE SPACE.                       
006540           07  FILLER      PIC X(12)   VALUE SPACE.                       
006541           07  FILLER      PIC X(3)    VALUE SPACE.                       
006542     03  FILLER              REDEFINES RRAD10-SPRAKGRP.                   
006543       05  RRAD10          OCCURS 2.                                      
006544           07  FILLER      PIC X(16).                                     
006545           07  RRAD10-KDTRDOK-TEXT                                        
006546                           PIC X(18).                                     
006547           07  FILLER      PIC X(37).                                     
006548           07  RRAD10-KDLBTYP                                             
006549                           PIC Z(3).                                      
006550           07  FILLER      PIC X.                                         
006551           07  RRAD10-BELBTYP                                             
006552                           PIC X(15).                                     
006553           07  FILLER      PIC X(4).                                      
006554           07  RRAD10-IDLBBET                                             
006555                           PIC X(12).                                     
006556           07  FILLER      PIC X(3).                                      
006557*- - - - - - - - - - - - - - - - - RUBRIKRAD 11                           
006558     03  RRAD11-SPRAKGRP.                                                 
006559       05  RRAD11-SVE.                                                    
006560           07  FILLER      PIC X(16)   VALUE 'ÖVRIGT'.                    
006561           07  FILLER      PIC X(80)   VALUE SPACE.                       
006562           07  FILLER      PIC X(19)   VALUE SPACE.                       
006563       05  RRAD11-ENG.                                                    
006564           07  FILLER      PIC X(16)   VALUE 'ALGEM.'.                    
006565           07  FILLER      PIC X(80)   VALUE SPACE.                       
006566           07  FILLER      PIC X(19)   VALUE SPACE.                       
006567     03  FILLER            REDEFINES RRAD11-SPRAKGRP.                     
006568       05  RRAD11          OCCURS 2.                                      
006569           07  FILLER      PIC X(16).                                     
006570           07  RRAD11-BETEXT-OVR                                          
006571                           PIC X(80).                                     
006572           07  FILLER      PIC X(19).                                     
006573*- - - - - - - - - - - - - - - - - RUBRIKRAD 12                           
006574     03  RRAD12-SPRAKGRP.                                                 
006575       05  RRAD12-SVE.                                                    
006576           07  FILLER      PIC X(16)   VALUE SPACE.                       
006577           07  FILLER      PIC X(80)   VALUE SPACE.                       
006578           07  FILLER      PIC X(19)   VALUE SPACE.                       
006579       05  RRAD12-SVE.                                                    
006580           07  FILLER      PIC X(20)   VALUE SPACE.                       
006581           07  FILLER      PIC X(80)   VALUE SPACE.                       
006582           07  FILLER      PIC X(19)   VALUE SPACE.                       
006583     03  FILLER            REDEFINES RRAD12-SPRAKGRP.                     
006584       05  RRAD12          OCCURS 2.                                      
006585           07  FILLER      PIC X(16).                                     
006586           07  RRAD12-BETEXT-OVR                                          
006587                           PIC X(80).                                     
006588           07  FILLER      PIC X(19).                                     
006589*- - - - - - - - - - - - - - - - - RUBRIKRAD 13                           
006590     03  RRAD13-SPRAKGRP.                                                 
006591       05  RRDAD13-SVE.                                                   
006592           07  FILLER      PIC X(16)   VALUE SPACE.                       
006593           07  FILLER      PIC X(80)   VALUE SPACE.                       
006594           07  FILLER      PIC X(19)   VALUE SPACE.                       
006595       05  RRDAD13-ENG.                                                   
006596           07  FILLER      PIC X(16)   VALUE SPACE.                       
006597           07  FILLER      PIC X(80)   VALUE SPACE.                       
006598           07  FILLER      PIC X(19)   VALUE SPACE.                       
006599     03  FILLER            REDEFINES RRAD13-SPRAKGRP.                     
006600       05  RRAD13          OCCURS 2.                                      
006601           07  FILLER      PIC X(16).                                     
006602           07  RRAD13-BETEXT-OVR                                          
006603                           PIC X(80).                                     
006604           07  FILLER      PIC X(19).                                     
006605*- - - - - - - - - - - - - - - - - RUBRIKRAD 14                           
006606     03  RRAD14-SPRAKGRP.                                                 
006607       05  RRAD14-SVE.                                                    
006608           07  FILLER      PIC X(21)   VALUE                              
006609               'LASTBÄRARINFORMATION:'.                                   
006610           07  FILLER      PIC X(94)   VALUE SPACE.                       
006611       05  RRAD14-ENG.                                                    
006612           07  FILLER      PIC X(21)   VALUE                              
006613               'TRANSPORTINFORMATIE.:'.                                   
006614           07  FILLER      PIC X(94)   VALUE SPACE.                       
006615     03  FILLER              REDEFINES RRAD14-SPRAKGRP.                   
006616       05  RRAD14          OCCURS 2.                                      
006617           07  FILLER      PIC X(21).                                     
006618           07  FILLER      PIC X(94).                                     
006619*- - - - - - - - - - - - - - - - - RUBRIKRAD 15                           
006620     03  RRAD15-SPRAKGRP.                                                 
006621       05  RRAD15-SVE.                                                    
006622           07  FILLER      PIC X(11)   VALUE 'NR     TYP '.               
006623           07  FILLER      PIC X(17)   VALUE SPACE.                       
006624           07  FILLER      PIC X(10)   VALUE 'BETECKNING'.                
006625           07  FILLER      PIC X(8)    VALUE SPACE.                       
006626           07  FILLER      PIC X(8)    VALUE 'SIGILLNR'.                  
006627           07  FILLER      PIC X(10)   VALUE SPACE.                       
006628           07  FILLER      PIC X(5)    VALUE ' TARA'.                     
006629           07  FILLER      PIC X(7)    VALUE SPACE.                       
006630           07  FILLER      PIC X(6)    VALUE 'ÖVRIGT'.                    
006631           07  FILLER      PIC X(33)   VALUE SPACE.                       
006632       05  RRAD15-ENG.                                                    
006633           07  FILLER      PIC X(11)   VALUE 'NR     TYPE'.               
006634           07  FILLER      PIC X(17)   VALUE SPACE.                       
006635           07  FILLER      PIC X(10)   VALUE 'OMSCHRIJV.'.                
006636           07  FILLER      PIC X(8)    VALUE SPACE.                       
006637           07  FILLER      PIC X(8)    VALUE 'ZEGEL   '.                  
006638           07  FILLER      PIC X(10)   VALUE SPACE.                       
006639           07  FILLER      PIC X(5)    VALUE 'TARRA'.                     
006640           07  FILLER      PIC X(7)    VALUE SPACE.                       
006641           07  FILLER      PIC X(6)    VALUE 'ALGEM.'.                    
006642           07  FILLER      PIC X(33)   VALUE SPACE.                       
006643      03  FILLER           REDEFINES RRAD15-SPRAKGRP.                     
006644        05 RRAD15          OCCURS 2.                                      
006645           07  FILLER      PIC X(11).                                     
006646           07  FILLER      PIC X(17).                                     
006647           07  FILLER      PIC X(10).                                     
006648           07  FILLER      PIC X(8).                                      
006649           07  FILLER      PIC X(8).                                      
006650           07  FILLER      PIC X(10).                                     
006651           07  FILLER      PIC X(5).                                      
006652           07  FILLER      PIC X(7).                                      
006653           07  FILLER      PIC X(6).                                      
006654           07  FILLER      PIC X(33).                                     
006655*- - - - - - - - - - - - - - - - - RUBRIKRAD 16                           
006656     03  RRAD16-SPRAKGRP.                                                 
006657       05  RRAD16-SVE.                                                    
006658           07  FILLER      PIC X(2)    VALUE ALL '.'.                     
006659           07  FILLER      PIC X(5)    VALUE SPACE.                       
006660           07  FILLER      PIC X(15)   VALUE ALL '.'.                     
006661           07  FILLER      PIC X(6)    VALUE SPACE.                       
006662           07  FILLER      PIC X(12)   VALUE ALL '.'.                     
006663           07  FILLER      PIC X(6)    VALUE SPACE.                       
006664           07  FILLER      PIC X(9)    VALUE ALL '.'.                     
006665           07  FILLER      PIC X(6)    VALUE SPACE.                       
006666           07  FILLER      PIC X(8)    VALUE '......,.'.                  
006667           07  FILLER      PIC X(6)    VALUE SPACE.                       
006668           07  FILLER      PIC X(30)   VALUE ALL '.'.                     
006669           07  FILLER      PIC X(10)   VALUE SPACE.                       
006670       05  RRAD16-ENG.                                                    
006671           07  FILLER      PIC X(2)    VALUE ALL '.'.                     
006672           07  FILLER      PIC X(5)    VALUE SPACE.                       
006673           07  FILLER      PIC X(15)   VALUE ALL '.'.                     
006674           07  FILLER      PIC X(6)    VALUE SPACE.                       
006675           07  FILLER      PIC X(12)   VALUE ALL '.'.                     
006676           07  FILLER      PIC X(6)    VALUE SPACE.                       
006677           07  FILLER      PIC X(9)    VALUE ALL '.'.                     
006678           07  FILLER      PIC X(6)    VALUE SPACE.                       
006679           07  FILLER      PIC X(8)    VALUE '......,.'.                  
006680           07  FILLER      PIC X(6)    VALUE SPACE.                       
006681           07  FILLER      PIC X(30)   VALUE ALL '.'.                     
006682           07  FILLER      PIC X(10)   VALUE SPACE.                       
006683      03  FILLER              REDEFINES RRAD16-SPRAKGRP.                  
006684        05 RRAD16          OCCURS 2.                                      
006685           07  FILLER      PIC X(2).                                      
006686           07  FILLER      PIC X(5).                                      
006687           07  FILLER      PIC X(15).                                     
006688           07  FILLER      PIC X(6).                                      
006689           07  FILLER      PIC X(12).                                     
006690           07  FILLER      PIC X(6).                                      
006691           07  FILLER      PIC X(9).                                      
006692           07  FILLER      PIC X(6).                                      
006693           07  FILLER      PIC X(8).                                      
006694           07  FILLER      PIC X(6).                                      
006695           07  FILLER      PIC X(30).                                     
006696           07  FILLER      PIC X(10).                                     
006697*- - - - - - - - - - - - - - - - - RUBRIKRAD 17                           
006698     03  RRAD17-SPRAKGRP.                                                 
006699       05  RRAD17-SVE.                                                    
006700           07  FILLER      PIC X(13)   VALUE 'SKEPPNINGSNR'.              
006701           07  FILLER      PIC X(7)    VALUE SPACE.                       
006702           07  FILLER      PIC X(2)    VALUE SPACE.                       
006703           07  FILLER      PIC X(3)    VALUE 'FK '.                       
006704           07  FILLER      PIC X(2)    VALUE SPACE.                       
006705           07  FILLER      PIC X(2)    VALUE SPACE.                       
006706           07  FILLER      PIC X(9)    VALUE 'DISTRIKT'.                  
006707           07  FILLER      PIC X(4)    VALUE SPACE.                       
006708           07  FILLER      PIC X       VALUE SPACE.                       
006709           07  FILLER      PIC X(7)    VALUE SPACE.                       
006710           07  FILLER      PIC X(66)   VALUE SPACE.                       
006711       05  RRAD17-ENG.                                                    
006712           07  FILLER      PIC X(13)   VALUE '   LAADNR  '.               
006713           07  FILLER      PIC X(7)    VALUE SPACE.                       
006714           07  FILLER      PIC X(2)    VALUE SPACE.                       
006715           07  FILLER      PIC X(3)    VALUE 'VK '.                       
006716           07  FILLER      PIC X(2)    VALUE SPACE.                       
006717           07  FILLER      PIC X(2)    VALUE SPACE.                       
006718           07  FILLER      PIC X(9)    VALUE 'DISTRIKT'.                  
006719           07  FILLER      PIC X(4)    VALUE SPACE.                       
006720           07  FILLER      PIC X       VALUE SPACE.                       
006721           07  FILLER      PIC X(7)    VALUE SPACE.                       
006722           07  FILLER      PIC X(66)   VALUE SPACE.                       
006723     03  FILLER            REDEFINES RRAD17-SPRAKGRP.                     
006724       05  RRAD17          OCCURS 2.                                      
006725           07  FILLER      PIC X(13).                                     
006726           07  RRAD17-IDLASTN                                             
006727                           PIC Z(7).                                      
006728           07  FILLER      PIC X(2).                                      
006729           07  FILLER      PIC X(3).                                      
006730           07  RRAD17-KDFRAKT                                             
006731                           PIC Z(2).                                      
006732           07  FILLER      PIC X(2).                                      
006733           07  FILLER      PIC X(9).                                      
006734           07  RRAD17-IDDISTR                                             
006735                           PIC Z(4).                                      
006736           07  FILLER      PIC X.                                         
006737           07  RRAD17-MFL  PIC X(7).                                      
006738           07  FILLER      PIC X(66).                                     
006739*- - - - - - - - - - - - - - - - - RUBRIKRAD 19                           
006740     03  RRAD19-SPRAKGRP.                                                 
006741       05  RRAD19-SVE.                                                    
006742           07  FILLER      PIC X(13)   VALUE 'GODSMÄRKNING'.              
006743           07  FILLER      PIC X(94)   VALUE SPACE.                       
006744           07  FILLER      PIC X(8)    VALUE SPACE.                       
006745       05  RRAD19-ENG.                                                    
006746           07  FILLER      PIC X(13)   VALUE 'KISTMERKEN  '.              
006747           07  FILLER      PIC X(94)   VALUE SPACE.                       
006748           07  FILLER      PIC X(8)    VALUE SPACE.                       
006749     03  FILLER              REDEFINES RRAD19-SPRAKGRP.                   
006750       05  RRAD19          OCCURS 2.                                      
006751           07  FILLER      PIC X(13).                                     
006752           07  RRAD19-BEGDSMRK                                            
006753                           PIC X(94).                                     
006754           07  FILLER      PIC X(8).                                      
006755*- - - - - - - - - - - - - - - - - RUBRIKRAD 20                           
006756     03  RRAD20-SPRAKGRP.                                                 
006757       05  RRAD20-SVE.                                                    
006758           07  FILLER      PIC X(21)   VALUE                              
006759               'DISTR KUNDNR ORDNR'.                                      
006760           07  FILLER      PIC X(31)   VALUE                              
006761               'KLI NR AVVIKELSER'.                                       
006762           07  FILLER      PIC X(40)   VALUE                              
006763               'ADRESS KKOD        L   B   H   BTO KG'.                   
006764           07  FILLER      PIC X(23)   VALUE                              
006765               'BTO M3 FARL PRODNR KLI '.                                 
006766       05  RRAD20-ENG.                                                    
006767           07  FILLER      PIC X(21)   VALUE                              
006768               'DISTR  KLANT  ORDER'.                                     
006769           07  FILLER      PIC X(31)   VALUE                              
006770               'KIS NR AFWIJKING '.                                       
006771           07  FILLER      PIC X(40)   VALUE                              
006772               'ADRES  KKODE       L   B   H   BTO KG'.                   
006773           07  FILLER      PIC X(23)   VALUE                              
006774               'BTO M3 GEV. PRODNR KIS '.                                 
006775     03  FILLER            REDEFINES RRAD20-SPRAKGRP.                     
006776        05  RRAD20         OCCURS 2.                                      
006777           07  FILLER      PIC X(21).                                     
006778           07  FILLER      PIC X(31).                                     
006779           07  FILLER      PIC X(40).                                     
006780           07  FILLER      PIC X(23).                                     
006781*- - - - - - - - - - - - - - - - - RUBRIKRAD 21                           
006782     03  RRAD21-SPRAKGRP.                                                 
006783       05  RRAD21-SVE.                                                    
006784           07  FILLER      PIC X(36)   VALUE                              
006785               'TOTALT FÖR ORDERN:     ANTAL KOLLIN'.                     
006786           07  FILLER      PIC X(5)    VALUE SPACE.                       
006787           07  FILLER      PIC X(3)    VALUE SPACE.                       
006788           07  FILLER      PIC X(10)   VALUE 'BRUTTO KG'.                 
006789           07  FILLER      PIC X(9)    VALUE SPACE.                       
006790           07  FILLER      PIC X(3)    VALUE SPACE.                       
006791           07  FILLER      PIC X(10)   VALUE 'BRUTTO M3'.                 
006792           07  FILLER      PIC X(9)    VALUE SPACE.                       
006793           07  FILLER      PIC X(30)   VALUE SPACE.                       
006794       05  RRAD21-ENG.                                                    
006795           07  FILLER      PIC X(36)   VALUE                              
006796               'TOTAAL PER ORDER:      AANTAL KIST '.                     
006797           07  FILLER      PIC X(5)    VALUE SPACE.                       
006798           07  FILLER      PIC X(3)    VALUE SPACE.                       
006799           07  FILLER      PIC X(10)   VALUE ' BRUTO KG'.                 
006800           07  FILLER      PIC X(9)    VALUE SPACE.                       
006801           07  FILLER      PIC X(3)    VALUE SPACE.                       
006802           07  FILLER      PIC X(10)   VALUE ' BRUTO M3'.                 
006803           07  FILLER      PIC X(9)    VALUE SPACE.                       
006804           07  FILLER      PIC X(30)   VALUE SPACE.                       
006805     03  FILLER              REDEFINES RRAD21-SPRAKGRP.                   
006806       05  RRAD21          OCCURS 2.                                      
006807           07  FILLER      PIC X(36).                                     
006808           07  RRAD21-ORDER-KVKOLLI                                       
006809                           PIC Z(4)9.                                     
006810           07  FILLER      PIC X(3).                                      
006811           07  FILLER      PIC X(10).                                     
006812           07  RRAD21-ORDER-VKORDBTO                                      
006813                           PIC Z(6)9.9.                                   
006814           07  FILLER      PIC X(3).                                      
006815           07  FILLER      PIC X(10).                                     
006816           07  RRAD21-ORDER-VLORDBTO                                      
006817                           PIC Z(4)9.9(3).                                
006818           07  FILLER      PIC X(30).                                     
006819*- - - - - - - - - - - - - - - - - RUBRIKRAD 22                           
006820     03  RRAD22-SPRAKGRP.                                                 
006821       05  RRAD22-SVE.                                                    
006822           07  FILLER      PIC X(36)   VALUE                              
006823               'TOTALT FÖR LASTNINGEN: ANTAL KOLLIN'.                     
006824           07  FILLER      PIC X(5)    VALUE SPACE.                       
006825           07  FILLER      PIC X(3)    VALUE SPACE.                       
006826           07  FILLER      PIC X(10)   VALUE 'BRUTTO KG'.                 
006827           07  FILLER      PIC X(9)    VALUE SPACE.                       
006828           07  FILLER      PIC X(3)    VALUE SPACE.                       
006829           07  FILLER      PIC X(10)   VALUE 'BRUTTO M3'.                 
006830           07  FILLER      PIC X(9)    VALUE SPACE.                       
006831           07  FILLER      PIC X(3)    VALUE SPACE.                       
006832           07  FILLER      PIC X(12)   VALUE 'ANTAL ORDER'.               
006833           07  FILLER      PIC X(3)    VALUE SPACE.                       
006834           07  FILLER      PIC X(12)   VALUE SPACE.                       
006835       05  RRAD22-ENG.                                                    
006836           07  FILLER      PIC X(36)   VALUE                              
006837               'TOTAAL PER LADING:     AANTAL KIST '.                     
006838           07  FILLER      PIC X(5)    VALUE SPACE.                       
006839           07  FILLER      PIC X(3)    VALUE SPACE.                       
006840           07  FILLER      PIC X(10)   VALUE ' BRUTO KG'.                 
006841           07  FILLER      PIC X(9)    VALUE SPACE.                       
006842           07  FILLER      PIC X(3)    VALUE SPACE.                       
006843           07  FILLER      PIC X(10)   VALUE ' BRUTO M3'.                 
006844           07  FILLER      PIC X(9)    VALUE SPACE.                       
006845           07  FILLER      PIC X(3)    VALUE SPACE.                       
006846           07  FILLER      PIC X(12)   VALUE 'AANT. ORDER'.               
006847           07  FILLER      PIC X(3)    VALUE SPACE.                       
006848           07  FILLER      PIC X(12)   VALUE SPACE.                       
006849     03  FILLER            REDEFINES RRAD22-SPRAKGRP.                     
006850       05  RRAD22          OCCURS 2.                                      
006851           07  FILLER      PIC X(36).                                     
006852           07  RRAD22-LASTN-KVKOLLI                                       
006853                           PIC Z(4)9.                                     
006854           07  FILLER      PIC X(3).                                      
006855           07  FILLER      PIC X(10).                                     
006856           07  RRAD22-LASTN-VKORDBTO                                      
006857                           PIC Z(6)9.9.                                   
006858           07  FILLER      PIC X(3).                                      
006859           07  FILLER      PIC X(10).                                     
006860           07  RRAD22-LASTN-VLORDBTO                                      
006861                           PIC Z(4)9.9(3).                                
006862           07  FILLER      PIC X(3).                                      
006863           07  FILLER      PIC X(12).                                     
006864           07  RRAD22-LASTN-KVORDER                                       
006865                           PIC Z(2)9.                                     
006866           07  FILLER      PIC X(12).                                     
006867*- - - - - - - - - - - - - - - - - RUBRIKRAD 23                           
006868     03  RRAD23-SPRAK-GRP.                                                
006869       05  RRAD23-SVE.                                                    
006870           07  FILLER      PIC X(9)    VALUE 'KVITTENS'.                  
006871           07  FILLER      PIC X(7)    VALUE SPACE.                       
006872           07  FILLER      PIC X(11)   VALUE 'TRUCKFÖRARE'.               
006873           07  FILLER      PIC X(21)   VALUE SPACE.                       
006874           07  FILLER      PIC X(8)    VALUE 'CHAUFFÖR'.                  
006875           07  FILLER      PIC X(24)   VALUE SPACE.                       
006876           07  FILLER      PIC X(9)    VALUE 'REGNR BIL'.                 
006877           07  FILLER      PIC X(26)   VALUE SPACE.                       
006878       05  RRAD23-ENG.                                                    
006879           07  FILLER      PIC X(9)    VALUE 'ONTVANG.'.                  
006880           07  FILLER      PIC X(7)    VALUE SPACE.                       
006881           07  FILLER      PIC X(11)   VALUE 'TRANSP.TEUR'.               
006882           07  FILLER      PIC X(21)   VALUE SPACE.                       
006883           07  FILLER      PIC X(8)    VALUE 'CHAUFF. '.                  
006884           07  FILLER      PIC X(24)   VALUE SPACE.                       
006885           07  FILLER      PIC X(9)    VALUE 'NR.VRACHT'.                 
006886           07  FILLER      PIC X(26)   VALUE SPACE.                       
006887     03  FILLER            REDEFINES RRAD23-SPRAK-GRP.                    
006888       05  RRAD23          OCCURS 2.                                      
006889           07  FILLER      PIC X(9).                                      
006890           07  FILLER      PIC X(7).                                      
006891           07  FILLER      PIC X(11).                                     
006892           07  FILLER      PIC X(21).                                     
006893           07  FILLER      PIC X(8).                                      
006894           07  FILLER      PIC X(24).                                     
006895           07  FILLER      PIC X(9).                                      
006896           07  FILLER      PIC X(26).                                     
006897*- - - - - - - - - - - - - - - - - RUBRIKRAD 24                           
006898     03  RRAD24-SPRAKGRP.                                                 
006899       05  RRAD24-SVE.                                                    
006900           07  FILLER      PIC X(15)   VALUE SPACE.                       
006901           07  FILLER      PIC X(30)   VALUE ALL '.'.                     
006902           07  FILLER      PIC X(2)    VALUE SPACE.                       
006903           07  FILLER      PIC X(30)   VALUE ALL '.'.                     
006904           07  FILLER      PIC X(2)    VALUE SPACE.                       
006905           07  FILLER      PIC X(16)   VALUE ALL '.'.                     
006906           07  FILLER      PIC X(20)   VALUE SPACE.                       
006907       05  RRAD24-ENG.                                                    
006908           07  FILLER      PIC X(15)   VALUE SPACE.                       
006909           07  FILLER      PIC X(30)   VALUE ALL '.'.                     
006910           07  FILLER      PIC X(2)    VALUE SPACE.                       
006911           07  FILLER      PIC X(30)   VALUE ALL '.'.                     
006912           07  FILLER      PIC X(2)    VALUE SPACE.                       
006913           07  FILLER      PIC X(16)   VALUE ALL '.'.                     
006914           07  FILLER      PIC X(20)   VALUE SPACE.                       
006915     03  FILLER            REDEFINES RRAD24-SPRAKGRP.                     
006916       05  RRAD24          OCCURS 2.                                      
006917           07  FILLER      PIC X(15).                                     
006918           07  FILLER      PIC X(30).                                     
006919           07  FILLER      PIC X(2).                                      
006920           07  FILLER      PIC X(30).                                     
006921           07  FILLER      PIC X(2).                                      
006922           07  FILLER      PIC X(16).                                     
006923           07  FILLER      PIC X(20).                                     
006924     EJECT                                                                
006925*- - - - - - - - - - - - - - - - - LISTRAD VILKA DETALJRADER              
006926*                                  LIGGER REDEFINES PÅ.                   
006927     03  LISTRAD.                                                         
006928         05  FILLER          PIC X(116).                                  
006929*- - - - - - - - - - - - - - - - - DETALJRAD 2                            
006930     03  DRAD2               REDEFINES LISTRAD.                           
006931         05  DRAD2-TOT-FALT      PIC X(1).                                
006932         05  DRAD2-IDDISTR       PIC Z(4).                                
006933         05  FILLER              PIC X.                                   
006934         05  DRAD2-IDKUNDNR      PIC Z(6).                                
006935         05  FILLER              PIC X(1).                                
006936         05  DRAD2-IDKUNDRF      PIC Z(4)9.                               
006937         05  FILLER              PIC X(1).                                
006938         05  DRAD2-IDKOLLI-1     PIC 9(5).                                
006939         05  FILLER              PIC X.                                   
006940         05  DRAD2-NR            PIC X(2).                                
006941         05  FILLER              PIC X(1).                                
006942         05  DRAD2-BETEXT-AVVIK  PIC X(14).                               
006943         05  FILLER              PIC X(1).                                
006944         05  DRAD2-ADRESS.                                                
006945             07  DRAD2-ADFLGEO   PIC X(3).                                
006946             07  FILLER          PIC X.                                   
006947             07  DRAD2-ADFLOMR   PIC Z(2)9.                               
006948             07  FILLER          PIC X.                                   
006949             07  DRAD2-ADRUTNIV  PIC Z(2)9.                               
006950             07  FILLER          PIC X.                                   
006951             07  DRAD2-ADVMODUL  PIC Z(3).                                
006952         05  FILLER              PIC X(1).                                
006953         05  DRAD2-KDKOLLI       PIC X(8).                                
006954         05  FILLER              PIC X(1).                                
006955         05  DRAD2-DIKOLLIL      PIC Z(4).                                
006956         05  FILLER              PIC X.                                   
006957         05  DRAD2-DIKOLLIB      PIC Z(3).                                
006958         05  FILLER              PIC X.                                   
006959         05  DRAD2-DIKOLLIH      PIC Z(3).                                
006960         05  FILLER              PIC X(1).                                
006961         05  DRAD2-VKORDBTO      PIC Z(5)9.9.                             
006962         05  FILLER              PIC X(1).                                
006963         05  DRAD2-VLORDBTO      PIC Z(3)9.9(3).                          
006964         05  FILLER              PIC X(1).                                
006965         05  DRAD2-KDFARLIG      PIC X(3).                                
006966         05  FILLER              PIC X(1).                                
006967         05  DRAD2-IDPRODNR      PIC Z(7).                                
006968         05  FILLER              PIC X.                                   
006969         05  DRAD2-IDKOLLI-2     PIC 9(5).                                
006970     EJECT                                                                
006971 01  ARB-RAD                     PIC X(132)  VALUE SPACE.                 
006972                                                                          
006973 01  RAD-HEAD.                                                            
006974     03  FILLER                    PIC X(67).                             
006975     03  RAD-TYP-IDSHIP            PIC X(25).                             
006976                                                                          
006977 01  RAD1.                                                                
006978     03  FILLER                    PIC X(67).                             
006979     03  RAD1-TIAAMMDD             PIC 9(06).                             
006980     03  RAD1-IDDISTR              PIC Z(4)9.                             
006981     03  FILLER                    PIC X(01).                             
006982     03  RAD1-IDSHIPM              PIC Z(06)9.                            
006983     03  FILLER                    PIC X(04).                             
006984     03  RAD1-IDTRPTNR             PIC Z(02)9.                            
006985     03  FILLER                    PIC X(01).                             
006986     03  RAD1-IDLBBET              PIC X(12).                             
006987     03  FILLER                    PIC X(02).                             
006988     03  RAD1-PAGE-NO              PIC Z(03).                             
006990                                                                          
007086                                                                          
007087 77  W-KDSPRAK                 PIC S9      COMP-3.                        
007088 77  W-SUORDV                  PIC S9(9)V9(2) VALUE ZERO COMP-3.          
007089 77  W-KDVALISO                PIC X(3)    VALUE SPACE.                   
007090 01  DUMMY-AREA                PIC X(50)   VALUE SPACE.                   
007091                                                                          
007092 01  W-PRINT-DOC-FLAG            PIC X(01)   VALUE 'N'.                   
007093     88  W-PRINT-NEW-DOC                     VALUE 'J'.                   
007094                                                                          
007095 01  W-TOTAL-FLAG                PIC X(01)   VALUE 'N'.                   
007096     88  W-PRINT-TOTAL                       VALUE 'J'.                   
007097                                                                          
007098 01  W-GRTOTAL-FLAG              PIC X(01)   VALUE 'N'.                   
007099     88  W-PRINT-GRTOTAL                     VALUE 'J'.                   
007100                                                                          
007101 01  W-PRINT-DEALER-FLAG         PIC X(01)   VALUE 'N'.                   
007102     88  W-PRINT-DEALER                      VALUE 'J'.                   
007103                                                                          
007104 01  W-KVLDISTR                  PIC S9(3)   VALUE 1 COMP-3.              
007105                                                                          
007106 01  W-TOT.                                                               
007107     03  W-TOTAL   OCCURS 2.                                              
007108         05  W-TOTAL-SUORDV            PIC S9(9)V9(2).                    
007109         05  W-TOTAL-VKORDBTO          PIC S9(6)V9(1).                    
007110         05  W-TOTAL-VKORDNTO          PIC S9(6)V9(1).                    
007111         05  W-TOTAL-VLORDBTO          PIC S9(6)V9(3).                    
007112         05  W-TOTAL-KOLLI-COUNT       PIC S9(6).                         
007113                                                                          
007114 77  W-ORDER-VKORDBTO        PIC S9(8)V9  VALUE ZERO COMP-3.              
007115 77  W-ORDER-VLORDBTO        PIC S9(6)V9(3) VALUE ZERO COMP-3.            
007116 77  W-ORDER-KVKOLLI         PIC S9(3)   VALUE ZERO  COMP-3.              
007117 77  W-LASTN-KVKOLLI         PIC S9(3)   VALUE ZERO  COMP-3.              
007118 77  W-SID-START             PIC S9(3)   VALUE ZERO  COMP-3.              
007119 77  W-SID-MAX               PIC S9(3)   VALUE +23   COMP-3.              
007120 77  W-LASTN-VKORDBTO        PIC S9(8)V9  VALUE ZERO COMP-3.              
007121 77  W-LASTN-VLORDBTO        PIC S9(6)V9(3) VALUE ZERO COMP-3.            
007122 77  W-LASTN-KVORDER         PIC S9(3)   VALUE ZERO  COMP-3.              
007123*                                                                         
007124 01  W-FARLIG-TEXT-SDC22        PIC X(4)   VALUE 'DANG'.                  
007125 01  W-FARLIG-TEXT-SDC24        PIC X(4)   VALUE 'PER.'.                  
007126 01  W-FARLIG-TEXT-SDC25        PIC X(4)   VALUE 'PER.'.                  
007127*                                                                         
007128 01  FILLER                     PIC X(16) VALUE 'LAST-W4067410'.          
007129*01  -COPY  W4067410     -PRE  LAST-                                      
007130                                                                          
007133     EJECT                                                                
007134*      --- VALID IDDC CODES                                               
007135*                                                                         
007136*01  -COPY WWDC99                                                         
007137     EJECT                                                                
007138                                                                          
007139 01  FILLER          PIC X(32)  VALUE  'LEDTEXT TABLE'.                   
007140* 01 -COPY W475W552                                                       
007141     EJECT                                                                
007142* 01 -COPY W475W553                                                       
007143     EJECT                                                                
007144* 01 -COPY W476W001                                                       
007145     EJECT                                                                
007146                                                                          
007147 01  GENERAL-SUBPROGRAMS.                                                 
007148*                                                                         
007149     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007150     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007151     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007152     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
007153     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
007154     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007155     SKIP2                                                                
007156*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007157                                                                          
007158 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007160 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  ERRTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700 77  KDRC-DISPLAY                PIC Z(5).                                
008000     EJECT                                                                
008011*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
008020 01  FILLER                      PIC X(16)   VALUE 'W006PRAR'.            
008030*01  -COPY W006PRAR                                                       
008031 01  FILLER                      PIC X(16)   VALUE 'W006PRT '.            
008032*01  -COPY W006PRT                                                        
008033 01  W-NYSIDA-RAD10          PIC S9(3)   VALUE +910  COMP-3.              
008040*                                        WRITE ON NEW LINE 10             
008050 01  W-IDPRTLST                  PIC X(8).                                
008060     SKIP2                                                                
008061 77  IDPRTLST-OPEN-SW            PIC X       VALUE 'N'.                   
008062     88  IDPRTLST-OPEN                       VALUE 'J'.                   
008063                                                                          
008070 01  FILLER                   PIC X(16)  VALUE '--WDB9-TAB----'.          
008080 01  TAB.                                                                 
008090     03 TAB-IDKUND              PIC X(10).                                
008091     03 TAB-ROW  OCCURS 10.                                               
008092        05 TAB-IDPRTLST         PIC X(8).                                 
008100        05 TAB-KVCOPIES-LAST    PIC 9(1).                                 
008101        05 TAB-IDDC-REC         PIC X(2).                                 
008110*    --- AREAS FOR IMS-SECTIONS                                           
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  KEYS-TO-DLI.                                                         
008700                                                                          
008701     03  W-IDSHIPM-X.                                                     
008702         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
008703                                                                          
008704     03  W-WDE111KY-X.                                                    
008705         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
008706         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
008707                                                                          
008712     03  W-WDE111KY-MIN.                                                  
008713         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
008714         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
008715                                                                          
008716     03  W-WDE111KY-MAX.                                                  
008717         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
008718         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
008719                                                                          
008723     03  W-IDPRODNR-X.                                                    
008724         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
008725                                                                          
008726     03  W-IDKOLLI-X.                                                     
008727         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
008728                                                                          
008729     03  W-SAMMANLAGD-NYCKEL-SKEPP.                                       
008730       05   W-IDDISTR-X.                                                  
008731         07  W-IDDISTR             PIC S9(5)         COMP-3.              
008732       05  W-IDKUNDNR-X.                                                  
008733         07  W-IDKUNDNR            PIC S9(7)         COMP-3.              
008734       05  W-IDDC-X.                                                      
008735         07  W-IDDC                PIC  X(2).                             
008736       05  W-IDSKEPPN-X.                                                  
008737         07  W-IDSKEPPN            PIC S9(7)         COMP-3.              
008738       05  W-IDLEVNR-X.                                                   
008739         07  W-IDLEVNR             PIC X(5)   VALUE SPACE.                
008742                                                                          
008743     03  W-WDGXKEY-4738-X.                                                
008744         05  FILLER              PIC X(4)     VALUE '4738'.               
008745         05  W-KDEMBTYP          PIC S9(3)    VALUE +0  COMP-3.           
008746         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
008747                                                                          
008748     03  W-WDGXKEY-4739-X.                                                
008749         05  FILLER              PIC X(4)     VALUE '4739'.               
008750         05  W-KDLBTYP           PIC S9(3)    VALUE +0  COMP-3.           
008751         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
008752                                                                          
008753     03  W-WDB901KY-MIN.                                                  
008754         05  W-WDB901-IDDC-MIN   PIC X(2)    VALUE LOW-VALUE.             
008755         05  W-WDB901-IDDISTR-MIN                                         
008756                                 PIC S9(5)   VALUE ZERO  COMP-3.          
008757         05  W-WDB901-IDKUND-MIN PIC X(10)   VALUE SPACE.                 
008758         05  W-IDDC-REC-MIN      PIC X(2)    VALUE LOW-VALUE.             
008759                                                                          
008760     03  W-WDB901KY-MAX.                                                  
008761         05  W-WDB901-IDDC-MAX   PIC X(2)    VALUE HIGH-VALUE.            
008762         05  W-WDB901-IDDISTR-MAX                                         
008763                                 PIC S9(5)   VALUE +99999 COMP-3.         
008764         05  W-WDB901-IDKUND-MAX PIC X(10)   VALUE SPACE.                 
008765         05  W-IDDC-REC-MAX      PIC X(2)    VALUE HIGH-VALUE.            
008766                                                                          
008767     03  W-WDB901KY-MIN2.                                                 
008768         05  W-WDB901-IDDC-MIN2  PIC X(2)    VALUE LOW-VALUE.             
008769         05  W-WDB901-IDDISTR-MIN2                                        
008770                                 PIC S9(5)   VALUE ZERO   COMP-3.         
008771          05  W-WDB901-IDKUND-MIN2                                        
008772                             PIC X(10)   VALUE SPACE.                     
008773          05  FILLER REDEFINES W-WDB901-IDKUND-MIN2.                      
008774              07 W-WDB901-IDKUNDNR-MIN2                                   
008775                                  PIC 9(6).                               
008776              07 FILLER           PIC X(4).                               
008777          05  FILLER REDEFINES W-WDB901-IDKUND-MIN2.                      
008778              07 W-WDB901-IDLEVNR-MIN2                                    
008779                                  PIC X(5).                               
008780              07 FILLER           PIC X(5).                               
008781          05  W-IDDC-REC-MIN2     PIC X(2)    VALUE LOW-VALUE.            
008782                                                                          
008783      03  W-WDB901KY-MAX2.                                                
008784          05  W-WDB901-IDDC-MAX2  PIC X(2)    VALUE HIGH-VALUE.           
008785          05  W-WDB901-IDDISTR-MAX2                                       
008786                                  PIC S9(5)   VALUE +99999 COMP-3.        
008787          05  W-WDB901-IDKUND-MAX2                                        
008788                                  PIC X(10)   VALUE SPACE.                
008789          05  FILLER REDEFINES W-WDB901-IDKUND-MAX2.                      
008790              07 W-WDB901-IDKUNDNR-MAX2                                   
008791                                  PIC 9(6).                               
008792              07 FILLER           PIC X(4).                               
008793          05  FILLER REDEFINES W-WDB901-IDKUND-MAX2.                      
008794              07 W-WDB901-IDLEVNR-MAX2                                    
008795                                  PIC X(5).                               
008796              07 FILLER           PIC X(5).                               
008797          05  W-IDDC-REC-MAX2     PIC X(2)     VALUE HIGH-VALUE.          
008798                                                                          
008799     03  W-WDB501KY-X.                                                    
008800       05  W-IDDC-WDB5             PIC X(2).                              
008801       05  W-KDFRAKT-WDB5          PIC S9(3)   COMP-3.                    
008802       05  W-IDGMT-WDB5.                                                  
008803          07 W-IDDISTR-WDB5        PIC S9(5)   COMP-3.                    
008804          07 W-IDKUNDNR-WDB5       PIC S9(7)   COMP-3.                    
008805                                                                          
008806     03  W-WDB501KY-DEF.                                                  
008807       05  W-IDDC-B5-DEF           PIC X(2).                              
008808       05  W-KDFRAKT-B5-DEF        PIC S9(3)   COMP-3.                    
008809       05  W-IDGMT-B5-DEF.                                                
008810         07 W-IDDISTR-B5-DEF       PIC S9(5)   COMP-3.                    
008811         07 W-IDKUNDNR-B5-DEF    PIC S9(7) COMP-3 VALUE +9999999.         
008812                                                                          
008813 01  WDG-4739.                                                            
008814*   03  WL473901 -COPY WDGX01                                             
008815*      04  -COPY WDGK4739                                                 
008816     EJECT                                                                
008817                                                                          
008820     SKIP2                                                                
008900*    --- STATUS-KOD FRÅN IMS                                              
009000 01  STATUS-WS                   PIC XX.                                  
009100     88  SEGMENT-FOUND                       VALUE '  '.                  
009200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009400     SKIP2                                                                
009500 01  GOOD-STATUSCODES.                                                    
009600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(128).                              
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010010                                                                          
010020 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
010030 01  SEND-AREA.                                                           
010040*    03  -COPY WZ01SEND                                                   
010050                                                                          
010091 01  SEND-RAD-STYRTECKEN.                                                 
010092     03  STYRTECKEN-RAD          PIC X.                                   
010093     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
010094                                                                          
010095 01  DAP-AREA-START              PIC X(24)   VALUE                        
010096                                             'DAP-AREA-START'.            
010097                                                                          
010100*    --- IMS FUNCTION CODES                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010601 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDE101-WDE111'.               
010602 01  DLI-IO-WDE101-11.                                                    
010603     03  DLI-IO-WDE101.                                                   
010604*        05  -COPY WDE101                                                 
010607     03  DLI-IO-WDE111.                                                   
010608*        05  -COPY WDE111                                                 
010609     EJECT                                                                
010610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
010611 01  DLI-IO-WDE121.                                                       
010612*    03  -COPY WDE121                                                     
010613 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
010614 01  DLI-IO-WDE601.                                                       
010615*    03  -COPY WDE601                                                     
010616 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
010617 01  DLI-IO-WDE611.                                                       
010618*    03  -COPY WDE611                                                     
010619 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4738'.                    
010620 01  DLI-IO-WDGX4738.                                                     
010630*    03  -COPY WDGX4738                                                   
010900     EJECT                                                                
010910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4739'.                    
010920 01  DLI-IO-WDGX4739.                                                     
010930*    03  -COPY WDGX4739                                                   
010940     EJECT                                                                
010950 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB901'.                      
010960 01  DLI-IO-WDB901.                                                       
010970*    03  -COPY WDB901                                                     
010980 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB501'.                      
010990 01  DLI-IO-WDB501.                                                       
010991*    03  -COPY WDB501                                                     
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W476TRPD                                                       
011201                                                                          
011202*01  -COPY W4067410                                                       
011203                                                                          
011204 01  ALT-PCB                     PIC X(32).                               
011205                                                                          
011207*01  -COPY W0008  -PRE WDE1-                                              
011208     05  FILLER                  PIC X.                                   
011209                                                                          
011210*01  -COPY W0008  -PRE WDE6-                                              
011211     05  FILLER                  PIC X.                                   
011212                                                                          
011213*01  -COPY W0008  -PRE WDR1-                                              
011220     05  FILLER                  PIC X.                                   
011230                                                                          
011240*01  -COPY W0008  -PRE WDB9-                                              
011250     05  FILLER                  PIC X.                                   
011260*01  -COPY W0008  -PRE WDB5-                                              
011270     05  FILLER                  PIC X.                                   
011300     EJECT                                                                
011401 PROCEDURE DIVISION  USING TRPD-W476TRPD W4067410                         
011402                           ALT-PCB                                        
011403                           WDE1-PCB WDE6-PCB WDR1-PCB                     
011404                           WDB9-PCB WDB5-PCB.                             
011406 MAIN SECTION.                                                            
011407     ENTRY 'DLITCBL' USING TRPD-W476TRPD W4067410                         
011408                           ALT-PCB                                        
011409                           WDE1-PCB WDE6-PCB WDR1-PCB                     
011410                           WDB9-PCB WDB5-PCB.                             
011700                                                                          
011800     PERFORM A-INIT                                                       
011900                                                                          
011938     PERFORM IMS-GU-WDE101                                                
011939     MOVE SHIP-IDDC           TO W-IDDC                                   
011940                                 W-IDDC-REC-MIN                           
011941                                 W-IDDC-REC-MIN2                          
011942                                 W-IDDC-REC-MAX                           
011943                                 W-IDDC-REC-MAX2                          
011950     PERFORM IMS-GNP-WDE111-DIST                                          
011951     MOVE SGMT-IDDISTR    TO W-WDE111-IDDISTR                             
011952     MOVE SGMT-IDKUNDNR   TO W-WDE111-IDKUNDNR                            
011953*                                                                         
011954* FÖR ATT HITTA KDFRAKT                                                   
011960     PERFORM IMS-GNP-WDE121                                               
011970     MOVE SKOLLI-IDPRODNR     TO W-IDPRODNR                               
011980     PERFORM IMS-GU-WDE601                                                
011990     PERFORM IMS-GU-WDE101                                                
012050*                                                                         
012060     MOVE YES       TO  W-PRINT-DOC-FLAG                                  
012061                                                                          
012062     PERFORM B-RED-GEM-LASTINFO                                           
012063     PERFORM C-SKRIV-GEM-LASTINFO                                         
012064     PERFORM IMS-GNP-WDE111-DIST                                          
012080     PERFORM UNTIL NOT SEGMENT-FOUND                                      
012082       PERFORM D-DEALER-DETAIL                                            
012084       PERFORM IMS-GNP-WDE111-DIST                                        
012085     END-PERFORM                                                          
012903                                                                          
012910*    IF W-PRINT-GRTOTAL                                                   
012911*      PERFORM S24-PRINT-GRTOTAL                                          
012920*    END-IF                                                               
012930     PERFORM S04-CLOSE-PRINTER                                            
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900     MOVE 'A-INIT'        TO W-SEKT                                       
014100                                                                          
014110     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
014200     ACCEPT TODAYS-DATE  FROM DATE                                        
014201     ACCEPT KLOCK-SLAG   FROM TIME                                        
014202     MOVE SPACE           TO TAB                                          
014203                             LISTRAD                                      
014210                                                                          
014230     MOVE TRPD-IDPRTLST   TO W-IDPRTLST                                   
014250     MOVE TRPD-IDSHIPM    TO W-IDSHIPM                                    
014251*    MOVE TRPD-PFDEF-OVR  TO PRT-PFDEF-OVR                                
014252     MOVE 'W40674'        TO PRT-PFDEF-OVR                                
014255     MOVE TRPD-IDDISTR    TO W-WDE111-IDDISTR                             
014256                             W-WDE111-IDDISTR-MIN                         
014257                             W-WDE111-IDDISTR-MAX                         
014260                                                                          
014300     MOVE NOO             TO W-PRINT-DOC-FLAG                             
014400                             W-PRINT-DEALER-FLAG                          
014401                             W-TOTAL-FLAG                                 
014402                                                                          
014410     MOVE 2               TO W-KDSPRAK                                    
014411     MOVE ZERO            TO W-PAGE-NO                                    
014412     MOVE W4067410        TO LAST-W4067410                                
014413                                                                          
014490     PERFORM S22-INIT-TOTAL                                               
014504                                                                          
014510     .                                                                    
014600     EJECT                                                                
014601 B-RED-GEM-LASTINFO  SECTION.                                             
014602     MOVE 'B-RED-GEM'       TO W-SEKT                                     
014603                                                                          
014604     MOVE SHIP-IDDC         TO WS-IDDC                                    
014605     IF CDC-SE OR DDC-SE                                                  
014606       MOVE 1               TO TYP                                        
014607     ELSE                                                                 
014608       MOVE 2               TO TYP                                        
014609     END-IF                                                               
014610     MOVE TODAYS-DATE-YEAR  TO RRAD1-DATUM-AA (TYP)                       
014611     MOVE TODAYS-DATE-MONTH TO RRAD1-DATUM-MM (TYP)                       
014612     MOVE TODAYS-DATE-DAY   TO RRAD1-DATUM-DD (TYP)                       
014613     ADD +1                 TO LLW-SIDNR                                  
014614     MOVE LLW-SIDNR         TO RRAD1-SIDA     (TYP)                       
014615                                                                          
014616     MOVE W-TIMMA           TO RRAD-KLOCK-HH  (TYP)                       
014617     MOVE W-MINUT           TO RRAD-KLOCK-MM  (TYP)                       
014618     MOVE W-IDSHIPM         TO RRAD2-IDLASTN  (TYP)                       
014619     MOVE VORD-KDFRAKT      TO RRAD2-KDFRAKT  (TYP)                       
014620     IF LAST-KVLDISTR  > 1                                                
014621         MOVE TEXT1 (TYP)   TO RRAD2-MFL      (TYP)                       
014622     END-IF                                                               
014623                                                                          
014624*    IF SPAR-LASTN-FLFAKT    = YES                                        
014625*        MOVE TEXT2 (TYP)   TO RRAD2-FAKTTEXT (TYP)                       
014626*    ELSE                                                                 
014627         MOVE SPACE         TO RRAD2-FAKTTEXT (TYP)                       
014628*    END-IF                                                               
014629                                                                          
014630     MOVE LAST-BEROUTE      TO RRAD5-BEROUTE  (TYP)                       
014631     MOVE LAST-IDTRANSP-NAMN TO                                           
014632                              RRAD6-IDTRANSP-NAMN (TYP)                   
014633     IF LAST-TIAVGANG   > ZERO                                            
014634          MOVE LAST-TIAVGANG TO W-DATUM                                   
014635          MOVE W-DATUM-AA   TO RRAD6-TIAVGANG-AA   (TYP)                  
014636          MOVE W-DATUM-MM   TO RRAD6-TIAVGANG-MM   (TYP)                  
014637          MOVE W-DATUM-DD   TO RRAD6-TIAVGANG-DD   (TYP)                  
014638          MOVE '.'          TO RRAD6-PUNKT1        (TYP)                  
014639                               RRAD6-PUNKT2        (TYP)                  
014640     END-IF                                                               
014641                                                                          
014642     MOVE LAST-BETEXT-HAEMT TO RRAD7-BETEXT-HAEMT  (TYP)                  
014643                                                                          
014644     IF LAST-TIHAEMT         > ZERO                                       
014645          MOVE LAST-TIHAEMT TO W-DATUM                                    
014646          MOVE W-DATUM-AA   TO RRAD7-TIHAEMT-AA    (TYP)                  
014647          MOVE W-DATUM-MM   TO RRAD7-TIHAEMT-MM    (TYP)                  
014648          MOVE W-DATUM-DD   TO RRAD7-TIHAEMT-DD    (TYP)                  
014649          MOVE '.'          TO RRAD7-PUNKT1        (TYP)                  
014650                               RRAD7-PUNKT2        (TYP)                  
014651     END-IF                                                               
014652                                                                          
014653     MOVE LAST-BETEXT-NEDK  TO RRAD8-BETEXT-NEDK   (TYP)                  
014654                                                                          
014655     IF LAST-TINEDK    > ZERO                                             
014656          MOVE LAST-TINEDK  TO W-DATUM                                    
014657          MOVE W-DATUM-AA   TO RRAD8-TINEDK-AA     (TYP)                  
014658          MOVE W-DATUM-MM   TO RRAD8-TINEDK-MM     (TYP)                  
014659          MOVE W-DATUM-DD   TO RRAD8-TINEDK-DD     (TYP)                  
014660          MOVE '.'          TO RRAD8-PUNKT1        (TYP)                  
014661                               RRAD8-PUNKT2        (TYP)                  
014662     END-IF                                                               
014663                                                                          
014664     MOVE LAST-IDBOKN          TO RRAD9-IDBOKN        (TYP)               
014665     EVALUATE LAST-KDTRDOK                                                
014666         WHEN 1                                                           
014667              MOVE TEXT4 (TYP) TO RRAD10-KDTRDOK-TEXT (TYP)               
014668         WHEN 2                                                           
014669              MOVE TEXT5 (TYP) TO RRAD10-KDTRDOK-TEXT (TYP)               
014670         WHEN 3                                                           
014671              MOVE TEXT6 (TYP) TO RRAD10-KDTRDOK-TEXT (TYP)               
014672     END-EVALUATE                                                         
014673                                                                          
014674     MOVE LOW-VALUE             TO NYCKEL-VALFRI                          
014675     IF LAST-KDLBTYP (1)   NOT = ZERO                                     
014676        MOVE LAST-KDLBTYP (1)   TO RRAD5-KDLBTYP (TYP)                    
014677                                   W-KDLBTYP                              
014679        PERFORM IMS-GU-LBTYP-4739                                         
014680                                                                          
014681        IF  SEGMENT-FOUND                                                 
014682            MOVE LBTYP-BELBTYP (1) TO RRAD5-BELBTYP (TYP)                 
014683        END-IF                                                            
014684     END-IF                                                               
014685                                                                          
014686     MOVE LOW-VALUE             TO NYCKEL-VALFRI                          
014687     IF LAST-KDLBTYP (2)   NOT = ZERO                                     
014688        MOVE LAST-KDLBTYP (2)   TO RRAD6-KDLBTYP (TYP)                    
014689                                   W-KDLBTYP                              
014691        PERFORM IMS-GU-LBTYP-4739                                         
014692                                                                          
014693        IF  SEGMENT-FOUND                                                 
014694          MOVE LBTYP-BELBTYP (1) TO RRAD6-BELBTYP (TYP)                   
014695        END-IF                                                            
014696     END-IF                                                               
014697                                                                          
014698     MOVE LOW-VALUE             TO NYCKEL-VALFRI                          
014699     IF LAST-KDLBTYP (3)   NOT = ZERO                                     
014700        MOVE LAST-KDLBTYP (3)   TO RRAD7-KDLBTYP (TYP)                    
014701                                   W-KDLBTYP                              
014703        PERFORM IMS-GU-LBTYP-4739                                         
014704                                                                          
014705        IF  SEGMENT-FOUND                                                 
014706            MOVE LBTYP-BELBTYP (1) TO RRAD7-BELBTYP (TYP)                 
014707        END-IF                                                            
014708     END-IF                                                               
014709                                                                          
014710     MOVE LOW-VALUE             TO NYCKEL-VALFRI                          
014711     IF LAST-KDLBTYP (4)   NOT = ZERO                                     
014712        MOVE LAST-KDLBTYP (4)   TO RRAD8-KDLBTYP (TYP)                    
014713                                   W-KDLBTYP                              
014715        PERFORM IMS-GU-LBTYP-4739                                         
014716                                                                          
014717        IF  SEGMENT-FOUND                                                 
014718            MOVE LBTYP-BELBTYP (1) TO RRAD8-BELBTYP (TYP)                 
014719        END-IF                                                            
014720     END-IF                                                               
014721                                                                          
014722     MOVE LOW-VALUE             TO NYCKEL-VALFRI                          
014723     IF LAST-KDLBTYP (5)   NOT = ZERO                                     
014724        MOVE LAST-KDLBTYP (5)   TO RRAD9-KDLBTYP (TYP)                    
014725                                   W-KDLBTYP                              
014727        PERFORM IMS-GU-LBTYP-4739                                         
014728                                                                          
014729        IF  SEGMENT-FOUND                                                 
014730            MOVE LBTYP-BELBTYP (1) TO RRAD9-BELBTYP (TYP)                 
014731        END-IF                                                            
014732     END-IF                                                               
014733                                                                          
014734     MOVE LOW-VALUE             TO NYCKEL-VALFRI                          
014735     IF LAST-KDLBTYP (6)   NOT = ZERO                                     
014736        MOVE LAST-KDLBTYP (6)   TO RRAD10-KDLBTYP (TYP)                   
014737                                   W-KDLBTYP                              
014739        PERFORM IMS-GU-LBTYP-4739                                         
014740                                                                          
014741        IF  SEGMENT-FOUND                                                 
014742            MOVE LBTYP-BELBTYP (1) TO RRAD10-BELBTYP (TYP)                
014743        END-IF                                                            
014744     END-IF                                                               
014745                                                                          
014746     MOVE LAST-IDLBBET (1)     TO RRAD5-IDLBBET     (TYP)                 
014747     MOVE LAST-IDLBBET (2)     TO RRAD6-IDLBBET     (TYP)                 
014748     MOVE LAST-IDLBBET (3)     TO RRAD7-IDLBBET     (TYP)                 
014749     MOVE LAST-IDLBBET (4)     TO RRAD8-IDLBBET     (TYP)                 
014750     MOVE LAST-IDLBBET (5)     TO RRAD9-IDLBBET     (TYP)                 
014751     MOVE LAST-IDLBBET (6)     TO RRAD10-IDLBBET    (TYP)                 
014752     MOVE LAST-BETEXT-OEVR (1) TO RRAD11-BETEXT-OVR (TYP)                 
014753                                                                          
014754     MOVE LAST-BETEXT-OEVR (2) TO RRAD12-BETEXT-OVR (TYP)                 
014755                                                                          
014756     MOVE LAST-BETEXT-OEVR (3) TO RRAD13-BETEXT-OVR (TYP)                 
014757                                                                          
014758     MOVE SGMT-IDDISTR         TO RRAD2-IDDISTR     (TYP)                 
014759                                                                          
014760     PERFORM BA-HITTA-PRINTER                                             
014761     .                                                                    
014762     EJECT                                                                
014763 BA-HITTA-PRINTER  SECTION.                                               
014764     MOVE 'BA-HITTA-PRINT'     TO W-SEKT                                  
014765                                                                          
014766     MOVE SGMT-IDDC            TO W-WDB901-IDDC-MIN                       
014767                                  W-WDB901-IDDC-MAX                       
014768                                  W-WDB901-IDDC-MIN2                      
014769                                  W-WDB901-IDDC-MAX2                      
014770     MOVE SGMT-IDDISTR         TO W-WDB901-IDDISTR-MIN                    
014771                                  W-WDB901-IDDISTR-MAX                    
014772                                  W-WDB901-IDDISTR-MIN2                   
014773                                  W-WDB901-IDDISTR-MAX2                   
014774     MOVE SGMT-IDKUNDNR        TO W-WDB901-IDKUNDNR-MIN2                  
014775                                  W-WDB901-IDKUNDNR-MAX2                  
014776                                                                          
014777     PERFORM S01B-GET-DIST-IDPRTLST                                       
014778     MOVE TAB-IDPRTLST (1)     TO W-IDPRTLST                              
014779*    MOVE 'QSE09049'           TO W-IDPRTLST                              
014780**   MOVE 'QSE09049'           TO PRT-IDLTERM                             
014781**   MOVE 2                    TO PRT-KDCALL                              
014782**   CALL W006PRT USING PRT-W006PRT                                       
014783**   IF PRT-KDSVAR = 'F'                                                  
014789**     MOVE 'REPRINT '         TO W-IDPRTLST                              
014790**   ELSE                                                                 
014791**     MOVE PRT-IDPRTLST       TO W-IDPRTLST                              
014792**   END-IF                                                               
014795     MOVE 'W40674  '           TO W-IDPRTLST                              
014796                                                                          
014797     PERFORM S03-OPEN-PRINTER                                             
014798     .                                                                    
014799     EJECT                                                                
014800 C-SKRIV-GEM-LASTINFO  SECTION.                                           
014801     MOVE 'C-SKRIV-GEM'        TO W-SEKT                                  
014802                                                                          
014803     MOVE PRT-NYSIDA-RAD4   TO PRT-RADSKIP                                
014804     MOVE RRAD1  (TYP)      TO ARB-RAD                                    
014805     PERFORM S21-PRINT-LINE                                               
014806     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
014807     MOVE RRAD-KLOCK-SLAG (TYP) TO ARB-RAD                                
014808     PERFORM S21-PRINT-LINE                                               
014809     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
014810**   MOVE RRAD2  (TYP)      TO ARB-RAD                                    
014811**   PERFORM S21-PRINT-LINE                                               
014812     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
014813     MOVE SHIP-IDLBBET      TO RRAD3-IDLBBET(TYP)                         
014815     MOVE SHIP-IDTRPTNR     TO RRAD3-IDTRPTNR(TYP)                        
014816     MOVE RRAD3  (TYP)      TO ARB-RAD                                    
014817     PERFORM S21-PRINT-LINE                                               
014818     MOVE RRAD4  (TYP)      TO ARB-RAD                                    
014819     PERFORM S21-PRINT-LINE                                               
014820     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
014821     MOVE RRAD5  (TYP)      TO ARB-RAD                                    
014822     PERFORM S21-PRINT-LINE                                               
014823     MOVE RRAD6  (TYP)      TO ARB-RAD                                    
014824     PERFORM S21-PRINT-LINE                                               
014825     MOVE RRAD7  (TYP)      TO ARB-RAD                                    
014826     PERFORM S21-PRINT-LINE                                               
014827     MOVE RRAD8  (TYP)      TO ARB-RAD                                    
014828     PERFORM S21-PRINT-LINE                                               
014829     MOVE RRAD9  (TYP)      TO ARB-RAD                                    
014830     PERFORM S21-PRINT-LINE                                               
014831     MOVE RRAD10 (TYP)      TO ARB-RAD                                    
014832     PERFORM S21-PRINT-LINE                                               
014833     MOVE RRAD11 (TYP)      TO ARB-RAD                                    
014834     PERFORM S21-PRINT-LINE                                               
014835     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
014836     MOVE RRAD12 (TYP)      TO ARB-RAD                                    
014837     PERFORM S21-PRINT-LINE                                               
014838     MOVE RRAD13 (TYP)      TO ARB-RAD                                    
014839     PERFORM S21-PRINT-LINE                                               
014840     MOVE PRT-AFTER-4       TO PRT-RADSKIP                                
014841     MOVE RRAD14 (TYP)      TO ARB-RAD                                    
014842     PERFORM S21-PRINT-LINE                                               
014843     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
014844     MOVE RRAD15 (TYP)      TO ARB-RAD                                    
014845     PERFORM S21-PRINT-LINE                                               
014846     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
014847     MOVE +1                TO IX                                         
014848     PERFORM UNTIL IX NOT < 6                                             
014849         MOVE RRAD16 (TYP)  TO ARB-RAD                                    
014850         PERFORM S21-PRINT-LINE                                           
014851         ADD +1             TO IX                                         
014852     END-PERFORM                                                          
014853     MOVE 40                TO LLW-RADANT                                 
014854     .                                                                    
014855     EJECT                                                                
014856                                                                          
014857 D-DEALER-DETAIL SECTION.                                                 
014858     MOVE 'D-DEALER-DET'    TO W-SEKT                                     
014859                                                                          
014860     MOVE YES           TO W-PRINT-DEALER-FLAG                            
014861*    MOVE IDKUNDNR-LEDTEXT                                                
014862*                       TO RAD2-IDKUNDNR-LEDTEXT                          
014863*    MOVE SGMT-IDKUNDNR TO RAD2-IDKUNDNR                                  
014864     PERFORM DB-RED-RUB-LASTINFO                                          
014865     PERFORM DA-PACKAGE-DETAIL                                            
014866     .                                                                    
014867     EJECT                                                                
014868                                                                          
014869 DA-PACKAGE-DETAIL SECTION.                                               
014870     MOVE 'DA-PACKAGE-DET'  TO W-SEKT                                     
014871                                                                          
014872     MOVE NOO            TO W-TOTAL-FLAG                                  
014873     MOVE SGMT-IDDISTR   TO W-WDE111-IDDISTR                              
014874     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
014875     PERFORM IMS-GNP-WDE121                                               
014876     PERFORM UNTIL NOT SEGMENT-FOUND                                      
014877       MOVE SKOLLI-IDPRODNR   TO W-IDPRODNR                               
014878                                                                          
014879       PERFORM UNTIL NOT SEGMENT-FOUND OR                                 
014880         SKOLLI-IDPRODNR NOT = W-IDPRODNR                                 
014881        MOVE SKOLLI-IDKOLLI    TO W-IDKOLLI                               
014882        PERFORM IMS-GU-WDE601                                             
014883        PERFORM IMS-GU-WDE611                                             
014884        PERFORM DAC-RED-RAD-KOLLIDATA                                     
014885        PERFORM IMS-GNP-WDE121                                            
014886       END-PERFORM                                                        
014887       PERFORM DAD-RED-TOTALRAD-ORDER                                     
014888                                                                          
014889     END-PERFORM                                                          
014890                                                                          
014891     PERFORM DAE-RED-TOTALRADER-LASTINFO                                  
014892                                                                          
014893*    IF W-PRINT-TOTAL                                                     
014894*       MOVE NOO                    TO W-TOTAL-FLAG                       
014895*       MOVE SPACES                 TO RAD7                               
014896*       MOVE PRODGRP-SUMMA-LEDTEXT (W-KDSPRAK)                            
014897*                                   TO RAD7-TOTAL-TEXT                    
014898*       MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                     
014899*                                   TO RAD7-KOLLI-TEXT                    
014900*                                                                         
014901*       MOVE W-TOTAL-VKORDBTO (1)   TO RAD7-TOTAL-VKORDBTO                
014902*       MOVE W-TOTAL-SUORDV (1)     TO RAD7-TOTAL-SUORDV                  
014903*       MOVE W-TOTAL-VKORDNTO (1)   TO RAD7-TOTAL-VKORDNTO                
014904*       MOVE W-TOTAL-VLORDBTO (1)   TO RAD7-TOTAL-VLORDBTO                
014905*       MOVE W-TOTAL-KOLLI-COUNT (1)                                      
014906*                                   TO RAD7-KOLLI-TOTAL                   
014907*       MOVE W-KDVALISO             TO RAD7-TOTAL-KDVALISO                
014908*                                                                         
014909*       MOVE ZERO                   TO W-TOTAL-SUORDV (1)                 
014910*                                      W-TOTAL-VKORDBTO (1)               
014911*                                      W-TOTAL-VKORDNTO (1)               
014912*                                      W-TOTAL-VLORDBTO (1)               
014913*                                      W-TOTAL-KOLLI-COUNT (1)            
014914**      MOVE SPACE               TO ARB-RAD                               
014915*       MOVE RAD7                TO ARB-RAD                               
014916*                                   SEND-RAD                              
014917*       ADD 1                    TO W-LINE-COUNT                          
014918*       MOVE PRT-AFTER-1         TO PRT-RADSKIP                           
014919*       MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
014920*       PERFORM S21-PRINT-LINE                                            
014921*       MOVE WS-SKIP1            TO STYRTECKEN-RAD                        
014922*                                                                         
014923*    END-IF                                                               
014924     .                                                                    
014925     EJECT                                                                
014926 DAC-RED-RAD-KOLLIDATA  SECTION.                                          
014927     MOVE 'DAC-RED-RAD'          TO W-SEKT                                
014928     MOVE SKOLLI-IDKOLLI           TO W-IDKOLLI                           
014929     MOVE SPACE                    TO DRAD2                               
014930     MOVE SKOLLI-IDKOLLI           TO DRAD2-IDKOLLI-1                     
014931                                      DRAD2-IDKOLLI-2                     
014932     MOVE '..'                     TO DRAD2-NR                            
014933                                                                          
014934     MOVE KOLLI-ADFLGEO            TO DRAD2-ADFLGEO                       
014935     MOVE KOLLI-ADFLOMR            TO DRAD2-ADFLOMR                       
014936     MOVE KOLLI-ADRUTNIV           TO DRAD2-ADRUTNIV                      
014937     MOVE KOLLI-ADVMODUL           TO DRAD2-ADVMODUL                      
014938                                                                          
014939     MOVE W-IDDC                   TO WS-IDDC                             
014940*    IF NDC OR SDC                                                        
014941     IF LAGG-UT-NYCKLAR                                                   
014942         MOVE SGMT-IDDISTR         TO DRAD2-IDDISTR                       
014943         MOVE SGMT-IDKUNDNR        TO DRAD2-IDKUNDNR                      
014944                                                                          
014945         MOVE SKOLLI-IDPRODNR      TO DRAD2-IDPRODNR                      
014946     END-IF                                                               
014947                                                                          
014948     MOVE SKOLLI-IDORDNR7          TO DRAD2-IDKUNDRF                      
014949     MOVE SKOLLI-KDKOLLI           TO DRAD2-KDKOLLI                       
014950     MOVE SKOLLI-DIKOLLIL          TO DRAD2-DIKOLLIL                      
014951     MOVE SKOLLI-DIKOLLIB          TO DRAD2-DIKOLLIB                      
014952     MOVE SKOLLI-DIKOLLIH          TO DRAD2-DIKOLLIH                      
014953     MOVE SKOLLI-VKORDBTO-KOLLI    TO DRAD2-VKORDBTO                      
014954     MOVE SKOLLI-VLORDBTO-KOLLI    TO DRAD2-VLORDBTO                      
014955     IF   SKOLLI-KDFARLIG-KOLLI  = 4                                      
014956     OR   SKOLLI-KDFARLIG-KOLLI  = 7                                      
014957          MOVE TEXT7 (TYP)         TO DRAD2-KDFARLIG                      
014958     END-IF                                                               
014959     IF SKOLLI-KDORDKL        > 3                                         
014960        MOVE '..............'      TO DRAD2-BETEXT-AVVIK                  
014961     ELSE                                                                 
014962        MOVE '.......... DAG'      TO DRAD2-BETEXT-AVVIK                  
014963     END-IF                                                               
014964                                                                          
014965     ADD +1                        TO W-ORDER-KVKOLLI                     
014966                                      W-LASTN-KVKOLLI                     
014967     ADD SKOLLI-VKORDBTO-KOLLI     TO W-ORDER-VKORDBTO                    
014968                                      W-LASTN-VKORDBTO                    
014969     ADD SKOLLI-VLORDBTO-KOLLI     TO W-ORDER-VLORDBTO                    
014970                                      W-LASTN-VLORDBTO                    
014971                                                                          
014972     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
014973     PERFORM S01-FLYTTA-RAD-TILL-PRINTER                                  
014974     .                                                                    
014975     EJECT                                                                
014976 DAD-RED-TOTALRAD-ORDER  SECTION.                                         
014977     MOVE 'DAD-RED-TOTAL'        TO W-SEKT                                
014978     MOVE W-ORDER-KVKOLLI        TO RRAD21-ORDER-KVKOLLI  (TYP)           
014979     MOVE W-ORDER-VKORDBTO       TO RRAD21-ORDER-VKORDBTO (TYP)           
014980     MOVE W-ORDER-VLORDBTO       TO RRAD21-ORDER-VLORDBTO (TYP)           
014981                                                                          
014982     MOVE PRT-AFTER-3            TO PRT-RADSKIP                           
014983     MOVE RRAD21 (TYP)           TO LISTRAD                               
014984     PERFORM S01-FLYTTA-RAD-TILL-PRINTER                                  
014985     MOVE ZERO                   TO W-ORDER-KVKOLLI                       
014986                                    W-ORDER-VKORDBTO                      
014987                                    W-ORDER-VLORDBTO                      
014988                                                                          
014989     IF  LAGG-UT-NYCKLAR                                                  
014990         MOVE SGMT-IDDISTR       TO DRAD2-IDDISTR                         
014991         MOVE SGMT-IDKUNDNR      TO DRAD2-IDKUNDNR                        
014993         MOVE SKOLLI-IDPRODNR    TO DRAD2-IDPRODNR                        
014994*        MOVE NOO                TO LAGG-UT-NYCKLAR-SW                    
014995     END-IF                                                               
014996     ADD +1                      TO W-LASTN-KVORDER                       
014997     .                                                                    
014998     EJECT                                                                
014999 DAE-RED-TOTALRADER-LASTINFO SECTION.                                     
015000     MOVE 'DAE-RED-TOTAL'   TO W-SEKT                                     
015001                                                                          
015002     MOVE W-LASTN-KVKOLLI   TO RRAD22-LASTN-KVKOLLI  (TYP)                
015003     MOVE W-LASTN-VKORDBTO  TO RRAD22-LASTN-VKORDBTO (TYP)                
015004     MOVE W-LASTN-VLORDBTO  TO RRAD22-LASTN-VLORDBTO (TYP)                
015005     MOVE W-LASTN-KVORDER   TO RRAD22-LASTN-KVORDER  (TYP)                
015006                                                                          
015007     MOVE PRT-AFTER-3       TO PRT-RADSKIP                                
015008     MOVE RRAD22 (TYP)      TO LISTRAD                                    
015009     IF  LLW-RADANT + PRT-RADSKIP + 6 > LLW-RADMAX                        
015010         MOVE LLW-NYSIDA    TO LLW-RADANT                                 
015011     END-IF                                                               
015012                                                                          
015013     PERFORM S01-FLYTTA-RAD-TILL-PRINTER                                  
015014     MOVE PRT-AFTER-3       TO PRT-RADSKIP                                
015015     MOVE RRAD23 (TYP)      TO LISTRAD                                    
015016     PERFORM S01-FLYTTA-RAD-TILL-PRINTER                                  
015017                                                                          
015018     MOVE PRT-AFTER-3       TO PRT-RADSKIP                                
015019     MOVE RRAD24 (TYP)      TO LISTRAD                                    
015020     PERFORM S01-FLYTTA-RAD-TILL-PRINTER                                  
015021                                                                          
015022     MOVE ZERO              TO W-LASTN-KVKOLLI                            
015023                               W-LASTN-VKORDBTO                           
015024                               W-LASTN-VLORDBTO                           
015025                               W-LASTN-KVORDER                            
015026     MOVE NOO               TO FLATERSTARTA                               
015027     .                                                                    
015028     EJECT                                                                
015029 DB-RED-RUB-LASTINFO  SECTION.                                            
015030     MOVE 'DB-RED-RUB'         TO W-SEKT                                  
015031     MOVE W-IDSHIPM            TO RRAD17-IDLASTN   (TYP)                  
015032     MOVE VORD-KDFRAKT         TO RRAD17-KDFRAKT   (TYP)                  
015033     IF   W-KVLDISTR         > 1                                          
015034         MOVE TEXT1 (TYP)      TO RRAD17-MFL       (TYP)                  
015035     END-IF                                                               
015036                                                                          
015037*    IF MFS-IDTRANS                 = '459H'                              
015038        MOVE SGMT-IDDISTR      TO RRAD17-IDDISTR   (TYP)                  
015039*    ELSE                                                                 
015040*       MOVE SGMT-IDDISTR      TO RRAD17-IDDISTR   (TYP)                  
015041*    END-IF                                                               
015042     .                                                                    
015043     EJECT                                                                
015198 S01-FLYTTA-RAD-TILL-PRINTER  SECTION.                                    
015199     MOVE 'S01-FLYTTA'              TO W-SEKT                             
015200     ADD PRT-RADSKIP                TO LLW-RADANT                         
015201     IF  LLW-RADANT > LLW-RADMAX                                          
015202         MOVE PRT-NYSIDA-RAD4       TO PRT-RADSKIP                        
015203         ADD +1                     TO LLW-SIDNR                          
015204                                       W-SID-START                        
015205         MOVE LLW-SIDNR             TO RRAD1-SIDA (TYP)                   
015206         MOVE RRAD1  (TYP)          TO ARB-RAD                            
015207         PERFORM S21-PRINT-LINE                                           
015208         MOVE +2                    TO PRT-RADSKIP                        
015209**       MOVE RRAD17 (TYP)          TO ARB-RAD                            
015210         MOVE RRAD3 (TYP)           TO ARB-RAD                            
015211         PERFORM S21-PRINT-LINE                                           
015212         PERFORM S01A-BEGMRK-FRAN-KUNDREG                                 
015213         MOVE W-BEGMRK              TO RRAD19-BEGDSMRK(TYP)               
015214         MOVE RRAD19 (TYP)          TO ARB-RAD                            
015215         PERFORM S21-PRINT-LINE                                           
015216         MOVE RRAD20 (TYP)          TO ARB-RAD                            
015217         PERFORM S21-PRINT-LINE                                           
015218         IF  DRAD2-TOT-FALT = SPACE                                       
015219             MOVE SGMT-IDDISTR     TO DRAD2-IDDISTR                       
015220             IF SGMT-IDKUNDNR    > ZERO                                   
015221                MOVE SGMT-IDKUNDNR TO DRAD2-IDKUNDNR                      
015222             ELSE                                                         
015223                MOVE SGMT-IDKUNDNR TO DRAD2-IDKUNDNR                      
015224             END-IF                                                       
015225                                                                          
015226             MOVE SKOLLI-IDPRODNR  TO DRAD2-IDPRODNR                      
015227             MOVE SKOLLI-IDORDNR7  TO DRAD2-IDKUNDRF                      
015228             MOVE YES           TO LAGG-UT-NYCKLAR-SW                     
015229         ELSE                                                             
015230             MOVE YES           TO LAGG-UT-NYCKLAR-SW                     
015231         END-IF                                                           
015232                                                                          
015233         MOVE +9                TO LLW-RADANT                             
015234                                                                          
015235         MOVE PRT-AFTER-2           TO PRT-RADSKIP                        
015236     END-IF                                                               
015237                                                                          
015238     MOVE LISTRAD                   TO ARB-RAD                            
015239     PERFORM S21-PRINT-LINE                                               
015240     MOVE PRT-AFTER-1               TO PRT-RADSKIP                        
015241     MOVE SPACE                     TO ARB-RAD                            
015242     IF W-SID-START                  > W-SID-MAX                          
015243        MOVE YES                    TO FLATERSTARTA                       
015244     END-IF                                                               
015245     .                                                                    
015246     EJECT                                                                
015247 S01A-BEGMRK-FRAN-KUNDREG SECTION.                                        
015248                                                                          
015249     MOVE SHIP-IDDC         TO W-IDDC-WDB5                                
015250                               W-IDDC-B5-DEF                              
015251     MOVE VORD-KDFRAKT      TO W-KDFRAKT-WDB5                             
015252                               W-KDFRAKT-B5-DEF                           
015253     MOVE SGMT-IDDISTR      TO W-IDDISTR-WDB5                             
015254                               W-IDDISTR-B5-DEF                           
015255     MOVE SGMT-IDKUNDNR     TO W-IDKUNDNR-WDB5                            
015256     PERFORM IMS-GU-WDB501                                                
015257                                                                          
015258     IF  SEGMENT-FOUND                                                    
015259       MOVE FK-BEGMRK       TO W-BEGMRK                                   
015260     END-IF                                                               
015261     .                                                                    
015262     EJECT                                                                
015263 S01B-GET-DIST-IDPRTLST  SECTION.                                         
015264     MOVE 'S01B-GET-DIST'           TO W-SEKT                             
015265     MOVE +1                       TO INDX                                
015266     MOVE SPACE                    TO TAB                                 
015267     IF W-IDLEVNR > SPACE                                                 
015268       MOVE SPACE                  TO W-WDB901-IDKUND-MIN2                
015269       MOVE SPACE                  TO W-WDB901-IDKUND-MAX2                
015270       MOVE W-IDLEVNR              TO W-WDB901-IDLEVNR-MIN2               
015271       MOVE W-IDLEVNR              TO W-WDB901-IDLEVNR-MAX2               
015272     END-IF                                                               
015273                                                                          
015274     PERFORM IMS-GU-WDB901                                                
015275                                                                          
015276     PERFORM UNTIL  NOT SEGMENT-FOUND                                     
015277                                                                          
015278       IF  DOK-IDKUND > SPACE                                             
015279       AND TAB-IDKUND = SPACE                                             
015280*........EXEPTIONS EXISTS, DEFAULT VALUES NOT USED                        
015281         MOVE +1                   TO INDX                                
015282       END-IF                                                             
015283                                                                          
015284       MOVE DOK-IDKUND             TO TAB-IDKUND                          
015285       MOVE DOK-IDPRTLST           TO TAB-IDPRTLST      (INDX)            
015286*      MOVE DOK-KVCOPIES-LAST      TO TAB-KVCOPIES-LAST (INDX)            
015287       MOVE 1                      TO TAB-KVCOPIES-LAST (INDX)            
015288       MOVE DOK-IDDC-REC           TO TAB-IDDC-REC      (INDX)            
015289                                                                          
015290       ADD +1                      TO INDX                                
015291                                                                          
015292       IF INDX > INDX-MAX2                                                
015293*........TABLE OVERFLOW                                                   
015294         MOVE DOK-IDDISTR          TO W-IDDISTR-DISP                      
015295         STRING 'MORE THAN 9 SEGMENTS IN WDB9:'                           
015296            DOK-IDDC ' ' W-IDDISTR-DISP ' ' DOK-IDKUND                    
015297               DELIMITED BY SIZE INTO ERRTEXT                             
015298         CALL FELLOG                                                      
015299       END-IF                                                             
015300                                                                          
015301       PERFORM IMS-GN-WDB901                                              
015302     END-PERFORM                                                          
015303     .                                                                    
015304     EJECT                                                                
015310 S03-OPEN-PRINTER  SECTION.                                               
015311     MOVE 'S03-OPEN-PRINTER'    TO W-SEKT                                 
015312                                                                          
015313     IF IDPRTLST-OPEN                                                     
015320       CONTINUE                                                           
015330     ELSE                                                                 
015331*      IF W-IDPRTLST NOT = 'REPRINT' OR                                   
015332*        PRT-PFDEF-OVR NOT = 'W47601  '                                   
015333*        CALL FELLOG                                                      
015334*      END-IF                                                             
015340       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
015350                           PRT-OPEN                                       
015360                           W-IDPRTLST                                     
015370                           ALT-PCB                                        
015380                           DUMMY-AREA                                     
015390                           DUMMY-AREA                                     
015391                                                                          
015392       MOVE YES         TO IDPRTLST-OPEN-SW                               
015393     END-IF                                                               
015394     .                                                                    
015395     EJECT                                                                
015396 S04-CLOSE-PRINTER  SECTION.                                              
015397     MOVE 'S04-CLOSE-PRINT'  TO W-SEKT                                    
015399     IF IDPRTLST-OPEN                                                     
015400*      IF W-IDPRTLST NOT = 'REPRINT' OR                                   
015401**       PRT-PFDEF-OVR = 'P1REPRIN'                                       
015402*        PRT-PFDEF-OVR NOT = 'W47601  '                                   
015403*        CALL FELLOG                                                      
015404*      END-IF                                                             
015405       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
015406                           PRT-CLOSE                                      
015407                           W-IDPRTLST                                     
015408                           ALT-PCB                                        
015409                           DUMMY-AREA                                     
015410                           DUMMY-AREA                                     
015411     END-IF                                                               
015412**   CALL FELLOG                                                          
015413     .                                                                    
015414     EJECT                                                                
015415                                                                          
015416 S21-PRINT-LINE SECTION.                                                  
015417     MOVE 'S21-PRINT-LINE'    TO W-SEKT                                   
015420                                                                          
015421     PERFORM S90-PUT-DOC-LINE                                             
015422*      IF W-IDPRTLST NOT = 'REPRINT' OR                                   
015423**       PRT-PFDEF-OVR = 'P1REPRIN'                                       
015424*        PRT-PFDEF-OVR NOT = 'W47601  '                                   
015425*        CALL FELLOG                                                      
015426*      END-IF                                                             
015430     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
015431                         PRT-WRITE                                        
015432                         W-IDPRTLST                                       
015433                         ALT-PCB                                          
015434                         PRT-RADSKIP                                      
015435                         ARB-RAD                                          
015438     .                                                                    
015440     EJECT                                                                
015441                                                                          
015450 S22-INIT-TOTAL SECTION.                                                  
015451     MOVE 'S22-INIT-TOTAL'     TO W-SEKT                                  
015460                                                                          
015461     MOVE 1 TO INDX                                                       
015470     PERFORM UNTIL INDX > 2                                               
015480       MOVE ZERO TO W-TOTAL-SUORDV (INDX)                                 
015490                    W-TOTAL-VKORDBTO (INDX)                               
015491                    W-TOTAL-VKORDNTO (INDX)                               
015492                    W-TOTAL-VLORDBTO (INDX)                               
015493                    W-TOTAL-KOLLI-COUNT (INDX)                            
015494       ADD 1     TO INDX                                                  
015495     END-PERFORM                                                          
015496                                                                          
015497     .                                                                    
015498     EJECT                                                                
015499                                                                          
015534                                                                          
015535*S24-PRINT-GRTOTAL SECTION.                                               
015536*                                                                         
015537*    MOVE NOO                   TO W-GRTOTAL-FLAG                         
015538*    MOVE SPACES                TO RAD8                                   
015539*                                  ARB-RAD                                
015540*                                  SEND-RAD                               
015541*    MOVE IDSHIPM-TOTAL-LEDTEXT TO RAD8-TOTAL-TEXT                        
015542*    MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
015543*    MOVE WS-SKIP2              TO STYRTECKEN-RAD                         
015544*    ADD 2                      TO W-LINE-COUNT                           
015545*    IF W-LINE-COUNT > W-LINE-MAX - W-LINE-GRTOTAL                        
015546*      PERFORM S25-PRINT-DISTR                                            
015547*      MOVE PRT-AFTER-1         TO PRT-RADSKIP                            
015548*      MOVE WS-SKIP1            TO STYRTECKEN-RAD                         
015549*      ADD 1                    TO W-LINE-COUNT                           
015550*    END-IF                                                               
015551**   MOVE SPACES                TO ARB-RAD                                
015552*    MOVE RAD8                  TO ARB-RAD                                
015553*                                  SEND-RAD                               
015554*    PERFORM S21-PRINT-LINE                                               
015555*    MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
015556*                                                                         
015557*    MOVE SPACES                TO RAD7                                   
015558*                                  ARB-RAD                                
015559*                                  SEND-RAD                               
015560*    MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                        
015561*                               TO RAD7-KOLLI-TEXT                        
015562*    MOVE W-TOTAL-VKORDBTO (2)  TO RAD7-TOTAL-VKORDBTO                    
015563*    MOVE W-TOTAL-SUORDV (2)    TO RAD7-TOTAL-SUORDV                      
015564*    MOVE W-TOTAL-VKORDBTO (2)  TO RAD7-TOTAL-VKORDBTO                    
015565*    MOVE W-TOTAL-VKORDNTO (2)  TO RAD7-TOTAL-VKORDNTO                    
015566*    MOVE W-TOTAL-VLORDBTO (2)  TO RAD7-TOTAL-VLORDBTO                    
015567*    MOVE W-TOTAL-KOLLI-COUNT (2)                                         
015568*                               TO RAD7-KOLLI-TOTAL                       
015569*    MOVE W-KDVALISO            TO RAD7-TOTAL-KDVALISO                    
015570*    MOVE RAD7                  TO ARB-RAD                                
015571*                                  SEND-RAD                               
015572*    MOVE PRT-AFTER-1           TO PRT-RADSKIP                            
015573*    MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
015574*    ADD 1                      TO W-LINE-COUNT                           
015575*    PERFORM S21-PRINT-LINE                                               
015576*    MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
015577*                                                                         
015578*    PERFORM S22-INIT-TOTAL                                               
015580*    MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
015581*    .                                                                    
015582*    EJECT                                                                
015583                                                                          
015584 S25-PRINT-DISTR SECTION.                                                 
015585     MOVE 'S25-PRINT-DIST'     TO W-SEKT                                  
015586                                                                          
015587     MOVE NOO                  TO W-PRINT-DOC-FLAG                        
015588                                                                          
015589     MOVE SPACE                      TO SEND-RAD                          
015590     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
015591     PERFORM S90-PUT-DOC-LINE                                             
015592     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
015593     PERFORM S90-PUT-DOC-LINE                                             
015594     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
015595     PERFORM S90-PUT-DOC-LINE                                             
015596                                                                          
015597     MOVE WS-TYP-IDSHIP         TO  RAD-TYP-IDSHIP                        
015598     MOVE  RAD-HEAD             TO  ARB-RAD                               
015599                                    SEND-RAD                              
015600     MOVE WS-SKIP1              TO  STYRTECKEN-RAD                        
015601     MOVE  PRT-NYSIDA-RAD7      TO  PRT-RADSKIP                           
015602     MOVE  7                    TO  LLW-RADANT                            
015610     PERFORM S21-PRINT-LINE                                               
015614                                                                          
015615     MOVE SPACE                      TO SEND-RAD                          
015616     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
015617     PERFORM S90-PUT-DOC-LINE                                             
015618     PERFORM S90-PUT-DOC-LINE                                             
015619                                                                          
015620     ADD 1                 TO W-PAGE-NO                                   
015621     MOVE SPACE            TO RAD1                                        
015622     MOVE SHIP-TISKEPPN    TO W-YYMMDD                                    
015623     MOVE W-YYMMDD         TO RAD1-TIAAMMDD                               
015624     MOVE SGMT-IDDISTR     TO RAD1-IDDISTR                                
015625     MOVE SHIP-IDSHIPM     TO RAD1-IDSHIPM                                
015626     MOVE SHIP-IDTRPTNR    TO RAD1-IDTRPTNR                               
015627     MOVE SHIP-IDLBBET     TO RAD1-IDLBBET                                
015628     MOVE W-PAGE-NO        TO RAD1-PAGE-NO                                
015629     MOVE RAD1             TO ARB-RAD                                     
015630                              SEND-RAD                                    
015631     MOVE PRT-AFTER-3      TO PRT-RADSKIP                                 
015632     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
015633     ADD 3                 TO LLW-RADANT                                  
015634     PERFORM S21-PRINT-LINE                                               
015635     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
015636                                                                          
015637     MOVE SPACE            TO ARB-RAD                                     
015638                              SEND-RAD                                    
015639     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
015640     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
015641     ADD 1                 TO LLW-RADANT                                  
015642     PERFORM S21-PRINT-LINE                                               
015643     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
015644     .                                                                    
015645     EJECT                                                                
015650                                                                          
016108 S90-PUT-DOC-LINE SECTION.                                                
016109     IF TRPD-IDPGM = 'W4063600'                                           
016111       IF TRPD-FLSKRIV-ONDEM = YES                                        
016113         MOVE +1                          TO SEND-IDCOM                   
016114         MOVE 'PUT'                       TO SEND-KDFUNC                  
016115         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
016116         CALL WZ01SEND USING SEND-CONTROL-AREA                            
016117                             SEND-KVDLEN                                  
016118                             SEND-RAD-STYRTECKEN                          
016119         IF SEND-KDRC > ZERO                                              
016120           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
016121           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
016122           DELIMITED BY SIZE INTO ERRTEXT-STR                             
016123           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
016124         END-IF                                                           
016125       END-IF                                                             
016126     END-IF                                                               
016127     .                                                                    
016128     EJECT                                                                
016140* --- IMS SECTIONS  ---                                                   
016200                                                                          
016301     EJECT                                                                
016302 IMS-GU-WDE101 SECTION.                                                   
016303                                                                          
016304     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
016305          DELIMITED BY SIZE INTO SSA1                                     
016309     MOVE '    ' TO GOOD-STATUSCODES                                      
016310     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
016311     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
016312     PERFORM IMS-STATUSCHECK                                              
016313     .                                                                    
016314     EJECT                                                                
016316 IMS-GU-WDE111-DIST SECTION.                                              
016317                                                                          
016318     STRING 'WDE101  *PD(IDSHIPM  =' W-IDSHIPM-X ')'                      
016319          DELIMITED BY SIZE INTO SSA1                                     
016320     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
016321                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
016322          DELIMITED BY SIZE INTO SSA2                                     
016323     MOVE '  GE' TO GOOD-STATUSCODES                                      
016324     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101-11 SSA1 SSA2            
016325     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
016326     PERFORM IMS-STATUSCHECK                                              
016327     .                                                                    
016328     EJECT                                                                
016329 IMS-GNP-WDE111-DIST SECTION.                                             
016330                                                                          
016331     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
016332                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
016333          DELIMITED BY SIZE INTO SSA1                                     
016334     MOVE '  GE' TO GOOD-STATUSCODES                                      
016335     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
016336     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
016337     PERFORM IMS-STATUSCHECK                                              
016338     .                                                                    
016339     EJECT                                                                
016340 IMS-GNP-WDE121 SECTION.                                                  
016341                                                                          
016342     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
016343          DELIMITED BY SIZE INTO SSA1                                     
016344     MOVE 'WDE121  '          TO SSA2                                     
016345     MOVE '  GE' TO GOOD-STATUSCODES                                      
016346     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
016347     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
016348     PERFORM IMS-STATUSCHECK                                              
016349     .                                                                    
016350     EJECT                                                                
016373 IMS-GU-WDE601 SECTION.                                                   
016374                                                                          
016375     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
016376            DELIMITED BY SIZE INTO SSA1                                   
016377     MOVE '  GE'            TO GOOD-STATUSCODES                           
016378     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
016379                                                                          
016380     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
016381     PERFORM IMS-STATUSCHECK                                              
016382     .                                                                    
016383     EJECT                                                                
016384                                                                          
016385 IMS-GU-WDE611 SECTION.                                                   
016386                                                                          
016387     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
016388            DELIMITED BY SIZE INTO SSA1                                   
016389     MOVE '  GE'            TO GOOD-STATUSCODES                           
016390     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1                    
016391                                                                          
016392     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
016393     PERFORM IMS-STATUSCHECK                                              
016394     .                                                                    
016395     EJECT                                                                
016396 IMS-GU-WDGX4738 SECTION.                                                 
016397*        EMBALLAGE-TEXT                                                   
016398                                                                          
016399     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4738-X ')'                    
016400          DELIMITED BY SIZE INTO SSA1                                     
016401     MOVE 'WDGX4738  '     TO SSA2                                        
016402     MOVE '  GE'           TO GOOD-STATUSCODES                            
016403     CALL CBLTDLI USING GU WDR1-PCB DLI-IO-WDGX4738 SSA1 SSA2             
016404     MOVE WDR1-STATUS-CODE TO STATUS-WS                                   
016405     PERFORM IMS-STATUSCHECK                                              
016406     .                                                                    
016407     EJECT                                                                
016410 IMS-GU-LBTYP-4739 SECTION.                                               
016420*       LASTBÄRARTYP-TEXT                                                 
016430     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4739-X ')'                    
016440            DELIMITED BY SIZE INTO SSA1                                   
016450     MOVE 'WDGX4739 '       TO SSA2                                       
016460     MOVE '  GE'            TO GOOD-STATUSCODES                           
016470     CALL CBLTDLI USING GU WDR1-PCB DLI-IO-WDGX4739 SSA1 SSA2             
016480                                                                          
016490     MOVE WDR1-STATUS-CODE  TO STATUS-WS                                  
016491     PERFORM IMS-STATUSCHECK                                              
016492     .                                                                    
016493     SKIP3                                                                
016494 IMS-GU-WDB901 SECTION.                                                   
016496                                                                          
016497     STRING 'WDB901  (WDB901KY>=' W-WDB901KY-MIN2                         
016498                    '&WDB901KY<=' W-WDB901KY-MAX2                         
016499                    '+WDB901KY>=' W-WDB901KY-MIN                          
016500                    '&WDB901KY<=' W-WDB901KY-MAX  ')'                     
016501                   DELIMITED BY SIZE INTO SSA1                            
016502                                                                          
016503     MOVE '  GE'                   TO GOOD-STATUSCODES                    
016504     CALL CBLTDLI USING GU WDB9-PCB DLI-IO-WDB901 SSA1                    
016505     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
016506     PERFORM IMS-STATUSCHECK                                              
016507     .                                                                    
016508     EJECT                                                                
016509 IMS-GN-WDB901 SECTION.                                                   
016511                                                                          
016512     MOVE '  GEGB'                 TO GOOD-STATUSCODES                    
016513     CALL CBLTDLI USING GN WDB9-PCB DLI-IO-WDB901 SSA1                    
016514     MOVE WDB9-STATUS-CODE         TO STATUS-WS                           
016515     PERFORM IMS-STATUSCHECK                                              
016516     .                                                                    
016517     EJECT                                                                
016518 IMS-GU-WDB501 SECTION.                                                   
016519                                                                          
016520     STRING 'WDB501  (WDB501KY =' W-WDB501KY-X                            
016521                    '!WDB501KY =' W-WDB501KY-DEF ')'                      
016522            DELIMITED BY SIZE INTO SSA1                                   
016523     MOVE '  GE'                   TO GOOD-STATUSCODES                    
016524     CALL CBLTDLI USING GU WDB5-PCB DLI-IO-WDB501 SSA1                    
016525     MOVE WDB5-STATUS-CODE         TO STATUS-WS                           
016526     PERFORM IMS-STATUSCHECK                                              
016527     .                                                                    
016528     EJECT                                                                
016530 IMS-STATUSCHECK SECTION.                                                 
016600                                                                          
016700     SET STATUS-IX TO 1                                                   
016800     SEARCH GOOD-STATUS                                                   
016900       AT END                                                             
017010         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
017100           DELIMITED BY SIZE INTO ERRTEXT                                 
017200         DISPLAY ERRTEXT                                                  
017300         CALL FELLOG                                                      
017400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
017500         CONTINUE                                                         
017600     END-SEARCH                                                           
017700     .                                                                    
017710                                                                          
