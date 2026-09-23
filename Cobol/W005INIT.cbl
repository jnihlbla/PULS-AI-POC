000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W005INIT                                                 
000003 AUTHOR.         RICHARD.                                                 
000004 DATE-WRITTEN.   JAN   95.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION.                                                            
000008*        MPP INIT-MODUL.                                                  
000009*        GENERELLT SUBPROGRAM FÖR ATT SPARA OCH HÄMTA                     
000010*        ANVÄNDARINFO OCH NYCKLAR.                                        
000011*                                                                         
000012*    INDATA.                                                              
000013*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
000014*          MSGI-WMSGINIT                                                  
000015*          USEA-PCB                                                       
000016*                                                                         
000017*    UTDATA.                                                              
000018*        MSGI-WMSGINIT                                                    
000019*                                                                         
000020*    E'TRACKER: 4823800  2007-10 LDC-ROLL-OUT 11 ST NYA                   
000021*                                                                         
000022                                                                          
000023                                                                          
000024 ENVIRONMENT DIVISION.                                                    
000025                                                                          
000026 DATA DIVISION.                                                           
000027     EJECT                                                                
000028 WORKING-STORAGE SECTION.                                                 
000029*    -- CHECKED BY WY2000                                                 
000030                                                                          
000031 77  IDPGM                   PIC X(8)    VALUE 'W005INIT'.                
000032 77  W-COMPILED              PIC X(16)   VALUE SPACE.                     
000033 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
000034 77  JA                      PIC X       VALUE 'J'.                       
000035 77  NEJ                     PIC X       VALUE 'N'.                       
       77  TZRUL-HIT-SW                PIC X       VALUE 'N'.                   
           88  TZRUL-HIT-NO                        VALUE 'N'.                   
           88  TZRUL-HIT-YES                       VALUE 'J'.                   
000036 77  INDX                    PIC S9(9)   VALUE ZERO COMP SYNC.            
000037 77  WS-DIFF                 PIC S9(3)   VALUE ZERO COMP-3.               
000038 77  WS-DATE                 PIC 9(6)    VALUE ZERO.                      
000039 77  WS-TIME                 PIC 9(8)    VALUE ZERO.                      
000040 77  WS-IDTIDZON             PIC 9(2)    VALUE ZERO.                      
000041 77  WS-IDLTERM-USER         PIC X(8)    VALUE SPACE.                     
000042 77  WS-IDUSER-IDDC          PIC X(5)    VALUE SPACE.                     
000043 77  WS-IDTRANS              PIC X(4)    VALUE SPACE.                     
000044 77  WS-SPAR-USER            PIC X(200)  VALUE SPACE.                     
000045 77  WS-TILOKDAT             PIC X(6)    VALUE ZERO.                      
000046 77  WS-TILOKTID             PIC X(8)    VALUE ZERO.                      
000047                                                                          
000048 01  WS-IDUSER-LTERM.                                                     
000049   03  FILLER                PIC X(9)    VALUE 'IDUSER ='.                
000050   03  WS-IDUSER.                                                         
000051     05  WS-IDUSER-POS1-5.                                                
000052      07  WS-IDUSER-POS1-2.                                               
000053       09  WS-IDUSER-POS1    PIC X(1)    VALUE SPACE.                     
000054       09  WS-IDUSER-POS2    PIC X(1)    VALUE SPACE.                     
000055      07  WS-IDUSER-POS3-4.                                               
000056       09  WS-IDUSER-POS3    PIC X(1)    VALUE SPACE.                     
000057       09  WS-IDUSER-POS4    PIC X(1)    VALUE SPACE.                     
000058      07  WS-IDUSER-POS5     PIC X(1)    VALUE SPACE.                     
000059     05  FILLER              PIC X(3)    VALUE SPACE.                     
000060                                                                          
000061                                                                          
000062 01  DYNAMISKA-SUBPROGRAM.                                                
000063   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
000064   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
000065                                                                          
000066     EJECT                                                                
000067 01  TZ-FULL-DATE            PIC 9(8)    VALUE ZERO.                      
000068 01  FILLER REDEFINES TZ-FULL-DATE.                                       
000069   03  TZ-TILOKDAT-CENT      PIC 9(2).                                    
000070   03  TZ-TILOKDAT           PIC X(6).                                    
000071   03  FILLER REDEFINES TZ-TILOKDAT.                                      
000072     05  TZ-AA               PIC 9(2).                                    
000073     05  TZ-MM               PIC 9(2).                                    
000074     05  TZ-DD               PIC 9(2).                                    
000075                                                                          
000076 01  TZ-INT-DATE             PIC S9(9)   BINARY.                          
000077                                                                          
000078 01  TZ-TILOKTID             PIC X(4)    VALUE ZERO.                      
000079 01  FILLER REDEFINES TZ-TILOKTID.                                        
000080   03  TZ-TIME-HOUR          PIC 9(2).                                    
000081   03  TZ-TIME-MIN           PIC 9(2).                                    
000082                                                                          
000083 01  TZ-HOUR                 PIC S9(3)   COMP-3.                          
000084 01  TZ-MIN                  PIC S9(3)   COMP-3.                          
000085 01  TZ-DIFF                 PIC S9(3)   COMP-3.                          
000086                                                                          
000087 77  TZ-SKOTT-AR             PIC X(2)    VALUE SPACE.                     
000088   88  SKOTT-AR                              VALUE '00'                   
000089                '04' '08' '12' '16' '20' '24' '28' '32'                   
000090                '36' '40' '44' '48' '52' '56' '60' '64'                   
000091                '68' '72' '76' '80' '84' '88' '92' '96'.                  
000092                                                                          
000093     EJECT                                                                
000094 01  TZ-RULE-TABLE.                                                       
000095*                   EN MASKIN OCH TABELLSTOPP MÅSTE FINNAS                
000096*                                                                         
000097   03  TIDZON-MASKIN-2023.                                                
000098     05 FILLER               PIC X(16) VALUE '    230326231029'.          
000099     05 FILLER               PIC S9(3) VALUE +1  COMP-3.                  
000100   03  TIDZON-MASKIN-2024.                                                
000101     05 FILLER               PIC X(16) VALUE '    240331241027'.          
000102     05 FILLER               PIC S9(3) VALUE +1  COMP-3.                  
000103   03  TIDZON-MASKIN-2025.                                                
000104     05 FILLER               PIC X(16) VALUE '    250330251026'.          
000105     05 FILLER               PIC S9(3) VALUE +1  COMP-3.                  
000106   03  TIDZON-MASKIN-2026.                                                
000107     05 FILLER               PIC X(16) VALUE '    260329261025'.          
000108     05 FILLER               PIC S9(3) VALUE +1  COMP-3.                  
000109                                                                          
000110*       TIDSZON 02 = AUSTRALIA                                            
000150   03  TIDZON-02-2023.                                                    
000160     05 FILLER               PIC X(16) VALUE '02  221002230402'.          
000170     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000180   03  TIDZON-02-2024.                                                    
000190     05 FILLER               PIC X(16) VALUE '02  231001240407'.          
000200     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000300   03  TIDZON-02-2025.                                                    
000400     05 FILLER               PIC X(16) VALUE '02  241006250406'.          
000500     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000501   03  TIDZON-02-2026.                                                    
000502     05 FILLER               PIC X(16) VALUE '02  251005260405'.          
000503     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000504   03  TIDZON-02-2027.                                                    
000505     05 FILLER               PIC X(16) VALUE '02  261004270404'.          
000506     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000507                                                                          
000508*       TIDSZON 10 = FINLAND AND SA                                       
000509   03  TIDZON-10-DC85.                                                    
000510     05 FILLER               PIC X(16) VALUE '1085010101991231'.          
000511     05 FILLER               PIC S9(3) VALUE 0   COMP-3.                  
000509   03  TIDZON-10-2023.                                                    
000510     05 FILLER               PIC X(16) VALUE '10  230326231029'.          
000511     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000512   03  TIDZON-10-2024.                                                    
000513     05 FILLER               PIC X(16) VALUE '10  240331241027'.          
000514     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000515   03  TIDZON-10-2025.                                                    
000516     05 FILLER               PIC X(16) VALUE '10  250330251026'.          
000517     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000518   03  TIDZON-10-2026.                                                    
000519     05 FILLER               PIC X(16) VALUE '10  260329261025'.          
000520     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000521                                                                          
000522*       TIDSZON 11 = EUROPE EXCEPT FOR THE UK                             
000526   03  TIDZON-11-2023.                                                    
000527     05 FILLER               PIC X(16) VALUE '11  230326231029'.          
000528     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000529   03  TIDZON-11-2024.                                                    
000530     05 FILLER               PIC X(16) VALUE '11  240331241027'.          
000531     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000532   03  TIDZON-11-2025.                                                    
000533     05 FILLER               PIC X(16) VALUE '11  250330251026'.          
000534     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000535   03  TIDZON-11-2026.                                                    
000536     05 FILLER               PIC X(16) VALUE '11  260329261025'.          
000537     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000538                                                                          
000539*       TIDSZON 12 = THE UK                                               
000543   03  TIDZON-12-2023.                                                    
000544     05 FILLER               PIC X(16) VALUE '12  230326231029'.          
000545     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000546   03  TIDZON-12-2024.                                                    
000547     05 FILLER               PIC X(16) VALUE '12  240331241027'.          
000548     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000549   03  TIDZON-12-2025.                                                    
000550     05 FILLER               PIC X(16) VALUE '12  250330251026'.          
000551     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000552   03  TIDZON-12-2026.                                                    
000553     05 FILLER               PIC X(16) VALUE '12  260329261025'.          
000554     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000555                                                                          
000556*       TIDSZON 17 = USA (DC41, DC46, DC51)                               
000560   03  TIDZON-17-2023.                                                    
000561     05 FILLER               PIC X(16) VALUE '17  230312231105'.          
000562     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000563   03  TIDZON-17-2024.                                                    
000564     05 FILLER               PIC X(16) VALUE '17  240310241103'.          
000565     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000566   03  TIDZON-17-2025.                                                    
000567     05 FILLER               PIC X(16) VALUE '17  250309251102'.          
000568     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000569   03  TIDZON-17-2026.                                                    
000570     05 FILLER               PIC X(16) VALUE '17  260308261101'.          
000571     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000572                                                                          
000573*       TIDSZON 18 = CHICAGO(DC 45)                                       
000577   03  TIDZON-18-2023.                                                    
000578     05 FILLER               PIC X(16) VALUE '18  230312231105'.          
000579     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000580   03  TIDZON-18-2024.                                                    
000581     05 FILLER               PIC X(16) VALUE '18  240310241103'.          
000582     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000583   03  TIDZON-18-2025.                                                    
000584     05 FILLER               PIC X(16) VALUE '18  250309251102'.          
000585     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000586   03  TIDZON-18-2026.                                                    
000587     05 FILLER               PIC X(16) VALUE '18  260308261101'.          
000588     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000589                                                                          
000599*       TIDSZON 20 = USA (DC 43, DC44, DC47)                              
000603   03  TIDZON-20-2023.                                                    
000604     05 FILLER               PIC X(16) VALUE '20  230312231105'.          
000605     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000606   03  TIDZON-20-2024.                                                    
000607     05 FILLER               PIC X(16) VALUE '20  240310241103'.          
000608     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000609   03  TIDZON-20-2025.                                                    
000610     05 FILLER               PIC X(16) VALUE '20  250309251102'.          
000611     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000612   03  TIDZON-20-2026.                                                    
000613     05 FILLER               PIC X(16) VALUE '20  260308261101'.          
000614     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
000615                                                                          
000616   03  TIDZON-TABELL-STOPP.                                               
000617     05 FILLER               PIC X(16) VALUE '99ZZ000000000000'.          
000618     05 FILLER               PIC S9(3) VALUE +0  COMP-3.                  
000619                                                                          
000620*  -- LITE EXTRA SPACE SÅ BEHÖVER ANTAL OCCURS INTE VARA EXAKT            
000621   03  FILLER                PIC X(400) VALUE SPACE.                      
000622                                                                          
000623 01  FILLER REDEFINES TZ-RULE-TABLE.                                      
000624*  -- OCCURS TILLTAGET I ÖVERKANT - CA 40 ANVÄNDS                         
000625   03 TZRUL OCCURS 45.                                                    
000626     05  TZRUL-IDTIDZON      PIC 9(2).                                    
000627     05  TZRUL-IDDC          PIC X(2).                                    
000628     05  TZRUL-DAT-FOM       PIC 9(6).                                    
000629     05  TZRUL-DAT-TOM       PIC 9(6).                                    
000630     05  TZRUL-DIFF          PIC S9(3)            COMP-3.                 
000631                                                                          
000632                                                                          
000633     EJECT                                                                
000634*      --- VALID IDDC CODES                                               
000635*                                                                         
000636*01    -COPY WWDC99                                                       
000637*01    -COPY WWDCKONS                                                     
000638       EJECT                                                              
000639******************************************************************        
000640*                                                                         
000641*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000642*                                                                         
000643 01  IMS-WS.                                                              
000644   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
000645                                                                          
000646                                                                          
000647*                        **** STATUS-KOD FRÅN IMS                         
000648   03  STATUS-WS                 PIC XX.                                  
000649     88  SEGMENT-FINNS                       VALUE '  '.                  
000650     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000651     88  INSERTEN-OK                         VALUE '  '.                  
000652     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000653                                                                          
000654   03  GODK-STATUSKODER.                                                  
000655     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
000656                                                                          
000657                                                                          
000658 01  NYCKLAR-TILL-DLI.                                                    
000659   03  W-WDP701KY-X.                                                      
000660     05  W-IDUSER                PIC X(8)    VALUE 'EJ-INIT '.            
000661                                                                          
000662                                                                          
000663 01  SSA1                        PIC X(64).                               
000664     EJECT                                                                
000665 01  DLI-IO-AREA.                                                         
000666*  03   -COPY WDP701.                                                     
000667     EJECT                                                                
000668*                            IMS FUNKTIONSKODER                           
000669*01    -COPY W0003                                                        
000670     EJECT                                                                
000671 LINKAGE SECTION.                                                         
000672*01  -COPY WMSGINIT                                                       
000673     EJECT                                                                
000674*01  -COPY W0008 -PRE USEA-                                               
000675     05  FILLER                  PIC X.                                   
000676     EJECT                                                                
000677 PROCEDURE DIVISION USING  MSGI-WMSGINIT                                  
000678                           USEA-PCB.                                      
000679 STYR SECTION.                                                            
000680                                                                          
000681     PERFORM A-INIT-LAES-DB                                               
000682     IF MSGI-KDCALL = '001' OR '013'                                      
000683       PERFORM B-USER-BEH                                                 
000684       IF WS-IDUSER-IDDC NOT = 'WIDDC'                                    
000685         PERFORM C-KOLLA-KEY                                              
000686       END-IF                                                             
000687       PERFORM F-KOLLA-TIDZON                                             
000688     ELSE                                                                 
000689       IF MSGI-KDCALL = '002'                                             
000690         MOVE MSGI-SPAR-AREA     TO INIT-SPAR-AREA                        
000691         MOVE MSGI-IDTRANS       TO INIT-IDTRANS                          
000692         MOVE MSGI-KDMFSFOR      TO INIT-KDMFSFOR                         
000693       ELSE                                                               
000694         IF MSGI-KDCALL = '011'                                           
000695*                       KONV FRÅN MASKINTID TILL LOCAL-TIME               
000696             OR MSGI-KDCALL = '012'                                       
000697*                       KONV FRÅN LOCAL-TIME TILL MASKINTID               
000698           MOVE MSGI-TILOKDAT TO WS-TILOKDAT                              
000699           MOVE MSGI-TILOKTID (1:4) TO WS-TILOKTID                        
000700           PERFORM F-KOLLA-TIDZON                                         
000701         ELSE                                                             
000702           MOVE 'FEL KDCALL-TYP EJ = 1, 2, 11, 12 ' TO FELTEXT            
000703           CALL FELLOG                                                    
000704         END-IF                                                           
000705       END-IF                                                             
000706     END-IF                                                               
000707     PERFORM S-SECURITY                                                   
000708     PERFORM Z-FINIT                                                      
000709                                                                          
000710     MOVE ZERO TO RETURN-CODE                                             
000711     GOBACK                                                               
000712     .                                                                    
000713     EJECT                                                                
000714 A-INIT-LAES-DB SECTION.                                                  
000715                                                                          
000716     MOVE WHEN-COMPILED TO W-COMPILED                                     
000717     ACCEPT WS-DATE FROM DATE                                             
000718     ACCEPT WS-TIME FROM TIME                                             
000719     MOVE WS-DATE TO WS-TILOKDAT                                          
000720     MOVE WS-TIME TO WS-TILOKTID                                          
000721                                                                          
000722     MOVE MSGI-IDUSER TO WS-IDUSER-IDDC                                   
000723     IF WS-IDUSER-IDDC = 'WIDDC'                                          
000724       IF MSGI-IDUSER NOT = W-IDUSER                                      
000725         MOVE MSGI-IDUSER TO W-IDUSER                                     
000726         IF USEA-DBD-NAME = 'WDP7'                                        
000727           PERFORM IMS-GET-WDP7-ROT                                       
000728         ELSE                                                             
000729           PERFORM IMS-GET-USEA-ROT                                       
000730         END-IF                                                           
000731         IF SEGMENT-SAKNAS                                                
000732           STRING MSGI-IDUSER ' SAKNAS PÅ USER-REG. '                     
000733             DELIMITED BY SIZE INTO FELTEXT                               
000734             CALL FELLOG                                                  
000735         END-IF                                                           
000736       END-IF                                                             
000737     ELSE                                                                 
000738       IF WS-IDUSER = SPACE                                               
000739         MOVE MSGI-IDUSER       TO WS-IDUSER      W-IDUSER                
000740         IF MSGI-IDLTERM-USER = SPACE OR ALL '+' OR LOW-VALUE             
000741           MOVE MSGI-IDUSER       TO WS-IDLTERM-USER                      
000742         ELSE                                                             
000743           MOVE MSGI-IDLTERM-USER TO WS-IDLTERM-USER                      
000744         END-IF                                                           
000745         MOVE MSGI-IDTRANS      TO WS-IDTRANS                             
000746         IF USEA-DBD-NAME = 'WDP7'                                        
000747           PERFORM IMS-GET-WDP7-ROT                                       
000748         ELSE                                                             
000749           PERFORM IMS-GET-USEA-ROT                                       
000750         END-IF                                                           
000751         IF SEGMENT-SAKNAS                                                
000752           PERFORM AA-NY-USER                                             
000753         ELSE                                                             
000754           PERFORM AB-OLD-USER                                            
000755         END-IF                                                           
000756         MOVE WS-DATE TO INIT-TIUPPDAT                                    
000757         MOVE WS-TIME TO INIT-TIUPPTID                                    
000758         MOVE INIT-USER TO WS-SPAR-USER                                   
000759         IF WS-IDUSER NOT = WS-IDLTERM-USER                               
000760           IF MSGI-KDCALL = '011' OR '012' OR '013'                       
000761*------------ MED DESSA KDCALL BEHÖVS INGEN UPPDATERING BARA              
000762*------------ HÄMTNING AV VISS DATA. DET BETYDER ATT RESP. PMS:S          
000763*------------ PSB:ER KAN HA PROCOPT=GOT                                   
000764             CONTINUE                                                     
000765           ELSE                                                           
000766             IF SEGMENT-SAKNAS                                            
000767               IF USEA-DBD-NAME = 'WDP7'                                  
000768                 PERFORM IMS-ISRT-WDP7-ROT                                
000769               ELSE                                                       
000770                 PERFORM IMS-ISRT-USEA-ROT                                
000771               END-IF                                                     
000772             ELSE                                                         
000773               PERFORM IMS-REPL-USEA                                      
000774             END-IF                                                       
000775           END-IF                                                         
000776           MOVE WS-IDLTERM-USER TO W-IDUSER                               
000777           IF USEA-DBD-NAME = 'WDP7'                                      
000778             PERFORM IMS-GET-WDP7-ROT                                     
000779           ELSE                                                           
000780             PERFORM IMS-GET-USEA-ROT                                     
000781           END-IF                                                         
000782           IF SEGMENT-SAKNAS                                              
000783             MOVE SPACE TO INIT-WDP701                                    
000784           END-IF                                                         
000785           MOVE WS-SPAR-USER TO INIT-USER                                 
000786           MOVE WS-IDLTERM-USER TO INIT-IDUSER                            
000787         END-IF                                                           
000788       ELSE                                                               
000789         IF MSGI-IDLTERM-USER = SPACE OR ALL '+' OR LOW-VALUE             
000790           MOVE MSGI-IDUSER       TO WS-IDLTERM-USER W-IDUSER             
000791         ELSE                                                             
000792           MOVE MSGI-IDLTERM-USER TO WS-IDLTERM-USER W-IDUSER             
000793         END-IF                                                           
000794         IF USEA-DBD-NAME = 'WDP7'                                        
000795           PERFORM IMS-GET-WDP7-ROT                                       
000796         ELSE                                                             
000797           PERFORM IMS-GET-USEA-ROT                                       
000798         END-IF                                                           
000799         IF SEGMENT-SAKNAS                                                
000800           MOVE SPACE TO INIT-WDP701                                      
000801         END-IF                                                           
000802         MOVE WS-SPAR-USER TO INIT-USER                                   
000803         MOVE WS-IDLTERM-USER TO INIT-IDUSER                              
000804       END-IF                                                             
000805     END-IF                                                               
000806     .                                                                    
000807     EJECT                                                                
000808 AA-NY-USER SECTION.                                                      
000809                                                                          
000810     MOVE SPACE       TO INIT-WDP701                                      
000811     MOVE MSGI-IDUSER TO INIT-IDUSER                                      
000812     MOVE 'STYLESTD'  TO INIT-IDCSS                                       
000813     MOVE WS-DATE     TO INIT-TIREGDAT-MPP                                
000814                                                                          
000815     PERFORM AAA-INIT-IDFTG                                               
000816     PERFORM AAB-INIT-IDTIDZON                                            
000817     PERFORM AAC-INIT-IDLAND-SPR                                          
000818     PERFORM AAD-INIT-IDRT-KEY                                            
000819     PERFORM AAE-INIT-IDSPRAK                                             
000820     PERFORM AAF-INIT-IDDC                                                
000821     PERFORM AAG-INIT-KDMATT                                              
000822     .                                                                    
000823     EJECT                                                                
000824 AAA-INIT-IDFTG SECTION.                                                  
000825                                                                          
000826     EVALUATE WS-IDUSER-POS3-4                                            
000827         WHEN 'US'       MOVE '53' TO INIT-IDFTG                          
000828         WHEN 'CA'       MOVE '54' TO INIT-IDFTG                          
000829         WHEN 'CN'       MOVE '60' TO INIT-IDFTG                          
000830         WHEN 'TH'       MOVE '63' TO INIT-IDFTG                          
000831         WHEN 'TW'       MOVE '64' TO INIT-IDFTG                          
000832         WHEN 'KR'       MOVE '65' TO INIT-IDFTG                          
000833         WHEN 'MY'       MOVE '66' TO INIT-IDFTG                          
000834         WHEN 'IN'       MOVE '67' TO INIT-IDFTG                          
000835         WHEN 'RU'       MOVE '81' TO INIT-IDFTG                          
000836         WHEN 'BR'       MOVE '82' TO INIT-IDFTG                          
000837         WHEN 'MX'       MOVE '83' TO INIT-IDFTG                          
000838         WHEN 'ZA'       MOVE '85' TO INIT-IDFTG                          
000839         WHEN 'TR'       MOVE '86' TO INIT-IDFTG                          
000840         WHEN 'AE'       MOVE '87' TO INIT-IDFTG                          
000841         WHEN OTHER      MOVE '57' TO INIT-IDFTG                          
000842     END-EVALUATE                                                         
000843                                                                          
000844     .                                                                    
000845     EJECT                                                                
000846 AAB-INIT-IDTIDZON SECTION.                                               
000847                                                                          
000848     MOVE '11' TO INIT-IDTIDZON                                           
000849                                                                          
000850     IF WS-IDUSER-POS3-4 = 'US' OR 'CA' OR                                
000851        WS-IDUSER-POS1 = 'U'                                              
000852******** DEFAULT '17' FOR NA AND OVERRIDE LATER                           
000853******** DC41, DC45, DC46, DC51                                           
000854       MOVE '17' TO INIT-IDTIDZON                                         
000855       IF WS-IDUSER-POS5 = '3' OR '4' OR '7'                              
000856******** DC43, DC44, DC47                                                 
000857         MOVE '20' TO INIT-IDTIDZON                                       
000858       ELSE                                                               
000859         IF WS-IDUSER-POS5 = '5'                                          
000860******** DC45                                                             
000861           MOVE '18' TO INIT-IDTIDZON                                     
000862         END-IF                                                           
000863       END-IF                                                             
000864     END-IF                                                               
000865                                                                          
000866     IF WS-IDUSER-POS1-5 = 'PHL3A' OR 'PHL3B'                             
000867                        OR 'PHL2C' OR 'PHL2H'                             
000868        OR WS-IDUSER-POS3-4 = 'GB'                                        
000869       MOVE '12' TO INIT-IDTIDZON                                         
000870     END-IF                                                               
000871                                                                          
000872     IF WS-IDUSER-POS3-4 = 'AU'                                           
000873       MOVE '02' TO INIT-IDTIDZON                                         
000874     END-IF                                                               
000875                                                                          
000876     IF WS-IDUSER-POS3-4 = 'JP' OR 'KR' OR 'TW'                           
000877       MOVE '03' TO INIT-IDTIDZON                                         
000878     END-IF                                                               
000879                                                                          
000880     IF WS-IDUSER-POS3-4 = 'CN' OR 'MY'                                   
000881       MOVE '04' TO INIT-IDTIDZON                                         
000882     END-IF                                                               
000883                                                                          
000884     IF WS-IDUSER-POS3-4 = 'TH'                                           
000885       MOVE '05' TO INIT-IDTIDZON                                         
000886     END-IF                                                               
000887                                                                          
000888     IF WS-IDUSER-POS3-4 = 'IN'                                           
000889       MOVE '07' TO INIT-IDTIDZON                                         
000890     END-IF                                                               
000891                                                                          
000892     IF WS-IDUSER-POS3-4 = 'AE'                                           
000893       MOVE '08' TO INIT-IDTIDZON                                         
000894     END-IF                                                               
000895                                                                          
000896     IF WS-IDUSER-POS3-4 = 'RU'                                           
000897       MOVE '09' TO INIT-IDTIDZON                                         
000898     END-IF                                                               
000899                                                                          
000900     IF WS-IDUSER-POS3-4 = 'BR'                                           
000901       MOVE '15' TO INIT-IDTIDZON                                         
000902     END-IF                                                               
000903                                                                          
000904     IF WS-IDUSER-POS3-4 = 'MX'                                           
000905       MOVE '18' TO INIT-IDTIDZON                                         
000906     END-IF                                                               
000907     IF WS-IDUSER-POS3-4 = 'ZA'                                           
000908       MOVE '10' TO INIT-IDTIDZON                                         
000909     END-IF                                                               
000910     IF WS-IDUSER-POS3-4 = 'TR'                                           
000911       MOVE '10' TO INIT-IDTIDZON                                         
000912     END-IF                                                               
000913     .                                                                    
000914     EJECT                                                                
000915 AAC-INIT-IDLAND-SPR SECTION.                                             
000916                                                                          
000917     MOVE 'SE' TO INIT-IDLAND-SPR                                         
000918                                                                          
000919     IF WS-IDUSER-POS1 = 'U'                                              
000920        OR                                                                
000921       (WS-IDUSER-POS1-2 = 'RD' OR 'RB' OR                                
000922                           'MW' OR 'RN')                                  
000923        OR                                                                
000924       (WS-IDUSER-POS1-2 > 'R ' AND < 'RZ')                               
000925        OR                                                                
000926       (WS-IDUSER-POS3-4 = 'NL' OR 'BE' OR 'GB' OR                        
000927                           'DE' OR 'AT' OR 'ES' OR                        
000928                           'IT' OR 'US' OR 'CA' OR                        
000929                           'JP' OR 'AU' OR 'CN' OR                        
000930                           'PL' OR 'IN' OR 'TH' OR 'AE' OR                
000931                           'TW' OR 'KR' OR 'MY' OR 'RU' OR                
000932                           'L2' OR 'L3' OR 'L7')                          
000933       MOVE 'GB' TO INIT-IDLAND-SPR                                       
000934     END-IF                                                               
000935     .                                                                    
000936     EJECT                                                                
000937 AAD-INIT-IDRT-KEY SECTION.                                               
000938                                                                          
000939     MOVE 'CDC'   TO INIT-IDRT-KEY                                        
000940                                                                          
000941     IF WS-IDUSER-POS1-2 = 'RD' OR 'RB'                                   
000942        OR                                                                
000943       (WS-IDUSER-POS3-4 = 'NL' OR 'BE' OR 'DE')                          
000944       MOVE 'NL1' TO INIT-IDRT-KEY                                        
000945     END-IF                                                               
000946     IF WS-IDUSER-POS3-4 = 'AT'                                           
000947       MOVE 'WI1' TO INIT-IDRT-KEY                                        
000948     END-IF                                                               
000949     IF WS-IDUSER-POS1-5 = 'PHL3A'                                        
000950       MOVE 'GB3' TO INIT-IDRT-KEY                                        
000951     END-IF                                                               
000952     IF WS-IDUSER-POS1-5 = 'PHL3B'                                        
000953       MOVE 'GB7' TO INIT-IDRT-KEY                                        
000954     END-IF                                                               
000955     IF WS-IDUSER-POS1-5 = 'PHL3C'                                        
000956       MOVE 'DE2' TO INIT-IDRT-KEY                                        
000957     END-IF                                                               
000958     IF WS-IDUSER-POS1-5 = 'PHL3D'                                        
000959       MOVE 'IT2' TO INIT-IDRT-KEY                                        
000960     END-IF                                                               
000961     IF WS-IDUSER-POS1-5 = 'PHL3E'                                        
000962       MOVE 'DE4' TO INIT-IDRT-KEY                                        
000963     END-IF                                                               
000964     IF WS-IDUSER-POS1-5 = 'PHL3F'                                        
000965       MOVE 'IT3' TO INIT-IDRT-KEY                                        
000966     END-IF                                                               
000967     IF WS-IDUSER-POS1-5 = 'PHL3G'                                        
000968       MOVE 'DE7' TO INIT-IDRT-KEY                                        
000969     END-IF                                                               
000970     IF WS-IDUSER-POS1-5 = 'PHL3H'                                        
000971       MOVE 'CH1' TO INIT-IDRT-KEY                                        
000972     END-IF                                                               
000973     IF WS-IDUSER-POS1-5 = 'PHL3J'                                        
000974       MOVE 'NO2' TO INIT-IDRT-KEY                                        
000975     END-IF                                                               
000976     IF WS-IDUSER-POS1-5 = 'PHL3K'                                        
000977       MOVE 'DE9' TO INIT-IDRT-KEY                                        
000978     END-IF                                                               
000979     IF WS-IDUSER-POS1-5 = 'PHL3L'                                        
000980       MOVE 'BE1' TO INIT-IDRT-KEY                                        
000981     END-IF                                                               
000982     IF WS-IDUSER-POS1-5 = 'PHL3M'                                        
000983       MOVE 'D10' TO INIT-IDRT-KEY                                        
000984     END-IF                                                               
000985     IF WS-IDUSER-POS1-5 = 'PHL3N'                                        
000986       MOVE 'NL4' TO INIT-IDRT-KEY                                        
000987     END-IF                                                               
000988     IF WS-IDUSER-POS1-5 = 'PHL3O'                                        
000989       MOVE 'FI2' TO INIT-IDRT-KEY                                        
000990     END-IF                                                               
000991     IF WS-IDUSER-POS1-5 = 'PHL3P'                                        
000992       MOVE 'FR2' TO INIT-IDRT-KEY                                        
000993     END-IF                                                               
000994     IF WS-IDUSER-POS1-5 = 'PHL3R'                                        
000995       MOVE 'NL5' TO INIT-IDRT-KEY                                        
000996     END-IF                                                               
000997     IF WS-IDUSER-POS1-5 = 'PHL3S'                                        
000998       MOVE 'PL1' TO INIT-IDRT-KEY                                        
000999     END-IF                                                               
001000     IF WS-IDUSER-POS1-5 = 'PHL3T'                                        
001001       MOVE 'D11' TO INIT-IDRT-KEY                                        
001002     END-IF                                                               
001003     IF WS-IDUSER-POS1-5 = 'PHL2C'                                        
001004       MOVE 'GB5' TO INIT-IDRT-KEY                                        
001005     END-IF                                                               
001006     IF WS-IDUSER-POS1-5 = 'PHL2H'                                        
001007       MOVE 'UK2' TO INIT-IDRT-KEY                                        
001008     END-IF                                                               
001009     IF WS-IDUSER-POS1-5 = 'PHL2I'                                        
001010       MOVE 'DE3' TO INIT-IDRT-KEY                                        
001011     END-IF                                                               
001012     IF WS-IDUSER-POS1-5 = 'PHL2J'                                        
001013       MOVE 'DE5' TO INIT-IDRT-KEY                                        
001014     END-IF                                                               
001015     IF WS-IDUSER-POS1-5 = 'PHL2L'                                        
001016       MOVE 'DE8' TO INIT-IDRT-KEY                                        
001017     END-IF                                                               
001018     IF WS-IDUSER-POS1-5 = 'PHL2M'                                        
001019       MOVE 'NL3' TO INIT-IDRT-KEY                                        
001020     END-IF                                                               
001021     IF WS-IDUSER-POS1-5 = 'PHL2O'                                        
001022       MOVE 'CH?' TO INIT-IDRT-KEY                                        
001023     END-IF                                                               
001024     IF WS-IDUSER-POS3-4 = 'ES'                                           
001025       MOVE 'ES1' TO INIT-IDRT-KEY                                        
001026     END-IF                                                               
001027     IF WS-IDUSER-POS3-4 = 'IT'                                           
001028       MOVE 'IT1' TO INIT-IDRT-KEY                                        
001029     END-IF                                                               
001030     IF WS-IDUSER-POS3-4 = 'US'                                           
001031       MOVE 'US1' TO INIT-IDRT-KEY                                        
001032       IF WS-IDUSER-POS5 = '3'                                            
001033         MOVE 'US3' TO INIT-IDRT-KEY                                      
001034       END-IF                                                             
001035       IF WS-IDUSER-POS5 = '4'                                            
001036         MOVE 'US4' TO INIT-IDRT-KEY                                      
001037       END-IF                                                             
001038       IF WS-IDUSER-POS5 = '5'                                            
001039         MOVE 'US5' TO INIT-IDRT-KEY                                      
001040       END-IF                                                             
001041       IF WS-IDUSER-POS5 = '6'                                            
001042         MOVE 'US6' TO INIT-IDRT-KEY                                      
001043       END-IF                                                             
001044       IF WS-IDUSER-POS5 = '7'                                            
001045         MOVE 'US7' TO INIT-IDRT-KEY                                      
001046       END-IF                                                             
001047     END-IF                                                               
001048     IF WS-IDUSER-POS3-4 = 'CN'                                           
001049       MOVE 'CN1' TO INIT-IDRT-KEY                                        
001050       IF WS-IDUSER-POS5 = '2'                                            
001051         MOVE 'CN2' TO INIT-IDRT-KEY                                      
001052       END-IF                                                             
001053       IF WS-IDUSER-POS5 = '3'                                            
001054         MOVE 'CN3' TO INIT-IDRT-KEY                                      
001055       END-IF                                                             
001056       IF WS-IDUSER-POS5 = '4'                                            
001057         MOVE 'CN4' TO INIT-IDRT-KEY                                      
001058       END-IF                                                             
001059     END-IF                                                               
001060     IF WS-IDUSER-POS3-4 = 'CA'                                           
001061       MOVE 'CA1' TO INIT-IDRT-KEY                                        
001062     END-IF                                                               
001063     IF WS-IDUSER-POS3-4 = 'JP'                                           
001064       MOVE 'JP1' TO INIT-IDRT-KEY                                        
001065       IF WS-IDUSER-POS5 = 'A'                                            
001066         MOVE 'JP2' TO INIT-IDRT-KEY                                      
001067       END-IF                                                             
001068     END-IF                                                               
001069     IF WS-IDUSER-POS3-4 = 'AU'                                           
001070       MOVE 'AU1' TO INIT-IDRT-KEY                                        
001071     END-IF                                                               
001072     IF WS-IDUSER-POS3-4 = 'IN'                                           
001073       MOVE 'IN1' TO INIT-IDRT-KEY                                        
001074     END-IF                                                               
001075     IF WS-IDUSER-POS3-4 = 'TH'                                           
001076       MOVE 'TH1' TO INIT-IDRT-KEY                                        
001077     END-IF                                                               
001078     IF WS-IDUSER-POS3-4 = 'TW'                                           
001079       MOVE 'TW1' TO INIT-IDRT-KEY                                        
001080     END-IF                                                               
001081     IF WS-IDUSER-POS3-4 = 'KR'                                           
001082       MOVE 'KR1' TO INIT-IDRT-KEY                                        
001083     END-IF                                                               
001084     IF WS-IDUSER-POS3-4 = 'MY'                                           
001085       MOVE 'MY1' TO INIT-IDRT-KEY                                        
001086     END-IF                                                               
001087     IF WS-IDUSER-POS3-4 = 'RU'                                           
001088       MOVE 'RU1' TO INIT-IDRT-KEY                                        
001089     END-IF                                                               
001090     IF WS-IDUSER-POS3-4 = 'AE'                                           
001091       MOVE 'AE1' TO INIT-IDRT-KEY                                        
001092     END-IF                                                               
001093     IF WS-IDUSER-POS3-4 = 'BR'                                           
001094       MOVE 'BR1' TO INIT-IDRT-KEY                                        
001095     END-IF                                                               
001096     IF WS-IDUSER-POS3-4 = 'MX'                                           
001097       MOVE 'MX1' TO INIT-IDRT-KEY                                        
001098     END-IF                                                               
001099     IF WS-IDUSER-POS3-4 = 'ZA'                                           
001100       MOVE 'ZA1' TO INIT-IDRT-KEY                                        
001101     END-IF                                                               
001102     IF WS-IDUSER-POS3-4 = 'TR'                                           
001103       MOVE 'TR1' TO INIT-IDRT-KEY                                        
001104     END-IF                                                               
001105     IF WS-IDUSER-POS1-2 = 'MW' OR 'RN'                                   
001106       IF WS-IDUSER-POS3-4 = 'NO' OR '00'                                 
001107         MOVE 'NO1' TO INIT-IDRT-KEY                                      
001108       END-IF                                                             
001109     END-IF                                                               
001110     IF WS-IDUSER-POS1-2 > 'R ' AND < 'RZ'                                
001111       IF WS-IDUSER-POS1-2 = 'RS' AND                                     
001112          WS-IDUSER-POS3   = 'F'                                          
001113         MOVE 'FI1' TO INIT-IDRT-KEY                                      
001114       END-IF                                                             
001115     END-IF                                                               
001116     IF WS-IDUSER (1:5) = 'PHL1A'                                         
001117       MOVE 'SE1' TO INIT-IDRT-KEY                                        
001118     END-IF                                                               
001119     IF WS-IDUSER (1:5) = 'PHL1B'                                         
001120       MOVE 'SE2' TO INIT-IDRT-KEY                                        
001121     END-IF                                                               
001122     IF WS-IDUSER (1:5) = 'PHL1C'                                         
001123       MOVE 'SE3' TO INIT-IDRT-KEY                                        
001124     END-IF                                                               
001125     IF WS-IDUSER (1:5) = 'PHL1D'                                         
001126       MOVE 'SE4' TO INIT-IDRT-KEY                                        
001127     END-IF                                                               
001128     IF WS-IDUSER (1:5) = 'PHL1E'                                         
001129       MOVE 'SE5' TO INIT-IDRT-KEY                                        
001130     END-IF                                                               
001131     IF WS-IDUSER (1:5) = 'PHL1K'                                         
001132       MOVE 'SE?' TO INIT-IDRT-KEY                                        
001133     END-IF                                                               
001134     IF WS-IDUSER (1:5) = 'PHL7A'                                         
001135       MOVE 'CL1' TO INIT-IDRT-KEY                                        
001136     END-IF                                                               
001137     IF WS-IDUSER (1:5) = 'PHL7B'                                         
001138       MOVE 'CL2' TO INIT-IDRT-KEY                                        
001139     END-IF                                                               
001140     IF WS-IDUSER (1:5) = 'PHL7C'                                         
001141       MOVE 'CL3' TO INIT-IDRT-KEY                                        
001142     END-IF                                                               
001143     IF WS-IDUSER (1:5) = 'PHL7D'                                         
001144       MOVE 'CL4' TO INIT-IDRT-KEY                                        
001145     END-IF                                                               
001146     IF WS-IDUSER (1:5) = 'PHL7E'                                         
001147       MOVE 'CL5' TO INIT-IDRT-KEY                                        
001148     END-IF                                                               
001149     IF WS-IDUSER (1:5) = 'PHL7F'                                         
001150       MOVE 'CL6' TO INIT-IDRT-KEY                                        
001151     END-IF                                                               
001152     IF WS-IDUSER (1:5) = 'PHL7G'                                         
001153       MOVE 'CL7' TO INIT-IDRT-KEY                                        
001154     END-IF                                                               
001155     IF WS-IDUSER (1:5) = 'PHL7H'                                         
001156       MOVE 'CL8' TO INIT-IDRT-KEY                                        
001157     END-IF                                                               
001158     .                                                                    
001159     EJECT                                                                
001160 AAE-INIT-IDSPRAK SECTION.                                                
001161                                                                          
001162     IF WS-IDUSER-POS1 = 'U'                                              
001163       MOVE 'EN' TO INIT-IDSPRAK                                          
001164     ELSE                                                                 
001165       MOVE 'SV' TO INIT-IDSPRAK                                          
001166     END-IF                                                               
001167                                                                          
001168     IF WS-IDUSER-POS1-5 = 'PHL2M' OR 'PHL3N' OR                          
001169                           'PHL3R' OR                                     
001170        WS-IDUSER-POS1-2 = 'RD' OR 'RB'                                   
001171       MOVE 'NL' TO INIT-IDSPRAK                                          
001172     END-IF                                                               
001173     IF WS-IDUSER-POS3-4 = 'IT'  OR                                       
001174       (WS-IDUSER-POS1-5 = 'PHL3F' OR 'PHL3D')                            
001175       MOVE 'IT' TO INIT-IDSPRAK                                          
001176     END-IF                                                               
001177     IF WS-IDUSER-POS3-4 = 'GB'  OR                                       
001178       (WS-IDUSER-POS1-5 = 'PHL2C' OR 'PHL2H' OR                          
001179                           'PHL3A' OR 'PHL3B')                            
001180       MOVE 'UK' TO INIT-IDSPRAK                                          
001181     END-IF                                                               
001182     IF WS-IDUSER-POS1-5 = 'PHL2I' OR 'PHL2J' OR                          
001183                           'PHL2L' OR 'PHL3C' OR 'PHL3E' OR               
001184                           'PHL3G' OR 'PHL3K' OR 'PHL3M'                  
001185       MOVE 'DE' TO INIT-IDSPRAK                                          
001186     END-IF                                                               
001187     IF WS-IDUSER-POS1-5 = 'PHL3L'                                        
001188       MOVE 'BE' TO INIT-IDSPRAK                                          
001189     END-IF                                                               
001190     IF WS-IDUSER-POS1-5 = 'PHL2O' OR 'PHL3H'                             
001191       MOVE 'CH' TO INIT-IDSPRAK                                          
001192     END-IF                                                               
001193     IF WS-IDUSER-POS3-4 = 'ES'                                           
001194       MOVE 'ES' TO INIT-IDSPRAK                                          
001195     END-IF                                                               
001196     IF WS-IDUSER-POS3-4 = 'JP'                                           
001197       MOVE 'JP' TO INIT-IDSPRAK                                          
001198     END-IF                                                               
001199     IF WS-IDUSER-POS3-4 = 'CN' OR 'L7'                                   
001200       MOVE 'CN' TO INIT-IDSPRAK                                          
001201     END-IF                                                               
001202     IF WS-IDUSER-POS3-4 = 'IN' OR 'TH' OR 'TW' OR                        
001203                           'KR' OR 'MY' OR 'RU' OR 'AE'                   
001204       MOVE 'EN' TO INIT-IDSPRAK                                          
001205     END-IF                                                               
001206     .                                                                    
001207     EJECT                                                                
001208 AAF-INIT-IDDC SECTION.                                                   
001209                                                                          
001210     MOVE WC-CDC-SE   TO INIT-IDDC                                        
001211                                                                          
001212     IF WS-IDUSER-POS1-2 = 'RD' OR 'RB'                                   
001213       MOVE WC-SDC-NL TO INIT-IDDC                                        
001214     END-IF                                                               
001215     IF WS-IDUSER-POS3-4 = 'NL' OR 'BE' OR 'DE'                           
001216       MOVE WC-SDC-NL TO INIT-IDDC                                        
001217     END-IF                                                               
001218     IF WS-IDUSER-POS3-4 = 'AT'                                           
001219       MOVE WC-SDC-AT TO INIT-IDDC                                        
001220     END-IF                                                               
001221     IF WS-IDUSER-POS1-5 = 'PHL3A'                                        
001222       MOVE WC-LDC-GB-3A TO INIT-IDDC                                     
001223     END-IF                                                               
001224     IF WS-IDUSER-POS1-5 = 'PHL3B'                                        
001225       MOVE WC-LDC-GB-3B TO INIT-IDDC                                     
001226     END-IF                                                               
001227     IF WS-IDUSER-POS1-5 = 'PHL3C'                                        
001228       MOVE WC-LDC-DE-3C TO INIT-IDDC                                     
001229     END-IF                                                               
001230     IF WS-IDUSER-POS1-5 = 'PHL3D'                                        
001231       MOVE WC-LDC-IT-3D TO INIT-IDDC                                     
001232     END-IF                                                               
001233     IF WS-IDUSER-POS1-5 = 'PHL3E'                                        
001234       MOVE WC-LDC-DE-3E TO INIT-IDDC                                     
001235     END-IF                                                               
001236     IF WS-IDUSER-POS1-5 = 'PHL3F'                                        
001237       MOVE WC-LDC-IT-3F TO INIT-IDDC                                     
001238     END-IF                                                               
001239     IF WS-IDUSER-POS1-5 = 'PHL3G'                                        
001240       MOVE WC-LDC-DE-3G TO INIT-IDDC                                     
001241     END-IF                                                               
001242     IF WS-IDUSER-POS1-5 = 'PHL3H'                                        
001243       MOVE WC-LDC-CH-3H TO INIT-IDDC                                     
001244     END-IF                                                               
001245     IF WS-IDUSER-POS1-5 = 'PHL3J'                                        
001246       MOVE WC-LDC-NO-3J TO INIT-IDDC                                     
001247     END-IF                                                               
001248     IF WS-IDUSER-POS1-5 = 'PHL3K'                                        
001249       MOVE WC-LDC-DE-3K TO INIT-IDDC                                     
001250     END-IF                                                               
001251     IF WS-IDUSER-POS1-5 = 'PHL3L'                                        
001252       MOVE WC-LDC-BE-3L TO INIT-IDDC                                     
001253     END-IF                                                               
001254     IF WS-IDUSER-POS1-5 = 'PHL3M'                                        
001255       MOVE WC-LDC-DE-3M TO INIT-IDDC                                     
001256     END-IF                                                               
001257     IF WS-IDUSER-POS1-5 = 'PHL3N'                                        
001258       MOVE WC-LDC-NL-3N TO INIT-IDDC                                     
001259     END-IF                                                               
001260     IF WS-IDUSER-POS1-5 = 'PHL3O'                                        
001261       MOVE WC-LDC-FI-3O TO INIT-IDDC                                     
001262     END-IF                                                               
001263     IF WS-IDUSER-POS1-5 = 'PHL3P'                                        
001264       MOVE WC-LDC-FR-3P TO INIT-IDDC                                     
001265     END-IF                                                               
001266     IF WS-IDUSER-POS1-5 = 'PHL3R'                                        
001267       MOVE WC-LDC-NL-3R TO INIT-IDDC                                     
001268     END-IF                                                               
001269     IF WS-IDUSER-POS1-5 = 'PHL3S'                                        
001270       MOVE WC-LDC-PL-3S TO INIT-IDDC                                     
001271     END-IF                                                               
001272     IF WS-IDUSER-POS1-5 = 'PHL3T'                                        
001273       MOVE WC-LDC-DE-3T TO INIT-IDDC                                     
001274     END-IF                                                               
001275     IF WS-IDUSER-POS1-5 = 'PHL2C'                                        
001276       MOVE WC-LDC-GB-2C TO INIT-IDDC                                     
001277     END-IF                                                               
001278     IF WS-IDUSER-POS1-5 = 'PHL2H'                                        
001279       MOVE WC-LDC-GB-2H TO INIT-IDDC                                     
001280     END-IF                                                               
001281     IF WS-IDUSER-POS1-5 = 'PHL2I'                                        
001282       MOVE WC-LDC-DE-2I TO INIT-IDDC                                     
001283     END-IF                                                               
001284     IF WS-IDUSER-POS1-5 = 'PHL2J'                                        
001285       MOVE WC-LDC-DE-2J TO INIT-IDDC                                     
001286     END-IF                                                               
001287     IF WS-IDUSER-POS1-5 = 'PHL2L'                                        
001288       MOVE WC-LDC-DE-2L TO INIT-IDDC                                     
001289     END-IF                                                               
001290     IF WS-IDUSER-POS1-5 = 'PHL2M'                                        
001291       MOVE WC-LDC-NL-2M TO INIT-IDDC                                     
001292     END-IF                                                               
001293     IF WS-IDUSER-POS1-5 = 'PHL2O'                                        
001294       MOVE WC-LDC-CH-2O TO INIT-IDDC                                     
001295     END-IF                                                               
001296     IF WS-IDUSER-POS3-4 = 'ES'                                           
001297       MOVE WC-SDC-ES TO INIT-IDDC                                        
001298     END-IF                                                               
001299     IF WS-IDUSER-POS3-4 = 'IT'                                           
001300       MOVE WC-SDC-IT TO INIT-IDDC                                        
001301     END-IF                                                               
001302     IF WS-IDUSER-POS3-4 = 'US'                                           
001303       MOVE WC-NDC-US-RU   TO INIT-IDDC                                   
001304       IF WS-IDUSER-POS5 = '3'                                            
001305         MOVE WC-NDC-US-LA TO INIT-IDDC                                   
001306       END-IF                                                             
001307       IF WS-IDUSER-POS5 = '4'                                            
001308         MOVE WC-NDC-US-SE TO INIT-IDDC                                   
001309       END-IF                                                             
001310       IF WS-IDUSER-POS5 = '5'                                            
001311         MOVE WC-NDC-US-CH TO INIT-IDDC                                   
001312       END-IF                                                             
001313       IF WS-IDUSER-POS5 = '6'                                            
001314         MOVE WC-NDC-US-JA TO INIT-IDDC                                   
001315       END-IF                                                             
001316       IF WS-IDUSER-POS5 = '7'                                            
001317         MOVE WC-NDC-US-DA TO INIT-IDDC                                   
001318       END-IF                                                             
001319       IF WS-IDUSER-POS5 = '9'                                            
001320         MOVE WC-NDC-US-BAT TO INIT-IDDC                                  
001321       END-IF                                                             
001322     END-IF                                                               
001323     IF WS-IDUSER-POS3-4 = 'CA'                                           
001324       MOVE WC-NDC-CA TO INIT-IDDC                                        
001325     END-IF                                                               
001326     IF WS-IDUSER-POS3-4 = 'JP'                                           
001327       IF WS-IDUSER-POS5 = 'A'                                            
001328         MOVE WC-NDC-JP-6A TO INIT-IDDC                                   
001329       END-IF                                                             
001330       IF WS-IDUSER-POS5 = '1'                                            
001331         MOVE WC-NDC-JP-61 TO INIT-IDDC                                   
001332       END-IF                                                             
001333     END-IF                                                               
001334     IF WS-IDUSER-POS3-4 = 'BR'                                           
001335       MOVE WC-NDC-BR TO INIT-IDDC                                        
001336     END-IF                                                               
001337     IF WS-IDUSER-POS3-4 = 'MX'                                           
001338       MOVE WC-NDC-MX TO INIT-IDDC                                        
001339     END-IF                                                               
001340     IF WS-IDUSER-POS3-4 = 'ZA'                                           
001341       MOVE WC-NDC-ZA TO INIT-IDDC                                        
001342     END-IF                                                               
001343     IF WS-IDUSER-POS3-4 = 'TR'                                           
001344       MOVE WC-NDC-TR TO INIT-IDDC                                        
001345     END-IF                                                               
001346     IF WS-IDUSER-POS3-4 = 'AE'                                           
001347       MOVE WC-NDC-AE TO INIT-IDDC                                        
001348     END-IF                                                               
001349     IF WS-IDUSER-POS3-4 = 'AU'                                           
001350       MOVE WC-NDC-AU TO INIT-IDDC                                        
001351     END-IF                                                               
001352     IF WS-IDUSER-POS3-4 = 'IN'                                           
001353       MOVE WC-NDC-IN TO INIT-IDDC                                        
001354     END-IF                                                               
001355     IF WS-IDUSER-POS3-4 = 'TH'                                           
001356       MOVE WC-NDC-TH TO INIT-IDDC                                        
001357       IF WS-IDUSER-POS5 = '9'                                            
001358         MOVE WC-NDC-TH-93 TO INIT-IDDC                                   
001359       END-IF                                                             
001360     END-IF                                                               
001361     IF WS-IDUSER-POS3-4 = 'TW'                                           
001362       MOVE WC-NDC-TW TO INIT-IDDC                                        
001363     END-IF                                                               
001364     IF WS-IDUSER-POS3-4 = 'KR'                                           
001365       MOVE WC-NDC-KR TO INIT-IDDC                                        
001366     END-IF                                                               
001367     IF WS-IDUSER-POS3-4 = 'MY'                                           
001368       MOVE WC-NDC-MY TO INIT-IDDC                                        
001369     END-IF                                                               
001370     IF WS-IDUSER-POS3-4 = 'RU'                                           
001371       MOVE WC-NDC-RU-81 TO INIT-IDDC                                     
001372     END-IF                                                               
001373     IF WS-IDUSER-POS3-4 = 'CN'                                           
001374       MOVE WC-NDC-CN-71 TO INIT-IDDC                                     
001375       IF WS-IDUSER-POS5 = '2'                                            
001376         MOVE WC-NDC-CN-72 TO INIT-IDDC                                   
001377       END-IF                                                             
001378       IF WS-IDUSER-POS5 = '3'                                            
001379         MOVE WC-NDC-CN-73 TO INIT-IDDC                                   
001380       END-IF                                                             
001381       IF WS-IDUSER-POS5 = '4'                                            
001382         MOVE WC-NDC-CN-74 TO INIT-IDDC                                   
001383       END-IF                                                             
001384     END-IF                                                               
001385     IF WS-IDUSER-POS1-5 = 'PHL7A'                                        
001386       MOVE WC-LDC-CN-7A TO INIT-IDDC                                     
001387     END-IF                                                               
001388     IF WS-IDUSER-POS1-5 = 'PHL7B'                                        
001389       MOVE WC-LDC-CN-7B TO INIT-IDDC                                     
001390     END-IF                                                               
001391     IF WS-IDUSER-POS1-5 = 'PHL7C'                                        
001392       MOVE WC-LDC-CN-7C TO INIT-IDDC                                     
001393     END-IF                                                               
001394     IF WS-IDUSER-POS1-5 = 'PHL7D'                                        
001395       MOVE WC-LDC-CN-7D TO INIT-IDDC                                     
001396     END-IF                                                               
001397     IF WS-IDUSER-POS1-5 = 'PHL7E'                                        
001398       MOVE WC-LDC-CN-7E TO INIT-IDDC                                     
001399     END-IF                                                               
001400     IF WS-IDUSER-POS1-5 = 'PHL7F'                                        
001401       MOVE WC-LDC-CN-7F TO INIT-IDDC                                     
001402     END-IF                                                               
001403     IF WS-IDUSER-POS1-5 = 'PHL7G'                                        
001404       MOVE WC-LDC-CN-7G TO INIT-IDDC                                     
001405     END-IF                                                               
001406     IF WS-IDUSER-POS1-5 = 'PHL7H'                                        
001407       MOVE WC-LDC-CN-7H TO INIT-IDDC                                     
001408     END-IF                                                               
001409     .                                                                    
001410     EJECT                                                                
001411 AAG-INIT-KDMATT SECTION.                                                 
001412                                                                          
001413     IF WS-IDUSER-POS1 = 'U'                                              
001414         OR WS-IDUSER-POS3-4 = 'US'                                       
001415       MOVE 'U' TO INIT-KDMATT                                            
001416     ELSE                                                                 
001417       MOVE 'S' TO INIT-KDMATT                                            
001418     END-IF                                                               
001419     IF WS-IDUSER-POS1 = 'R'                                              
001420         AND WS-IDUSER-POS3-4 = 'US'                                      
001421       MOVE 'S' TO INIT-KDMATT                                            
001422     END-IF                                                               
001423     .                                                                    
001424     EJECT                                                                
001425 AB-OLD-USER SECTION.                                                     
001426                                                                          
001427     IF WS-IDUSER-POS1-2 = 'RS' AND                                       
001428        WS-IDUSER-POS3   = 'F'                                            
001429       MOVE 'FI1' TO INIT-IDRT-KEY                                        
001430     END-IF                                                               
001431     IF WS-IDUSER (1:4)  = 'RN00' OR 'MWNO'                               
001432       MOVE 'NO1' TO INIT-IDRT-KEY                                        
001433     END-IF                                                               
001434     IF WS-IDUSER (1:5) = 'PHL1A'                                         
001435         MOVE 'SE1' TO INIT-IDRT-KEY                                      
001436     ELSE                                                                 
001437       IF WS-IDUSER (1:5) = 'PHL1B'                                       
001438         MOVE 'SE2' TO INIT-IDRT-KEY                                      
001439       END-IF                                                             
001440     END-IF                                                               
001441     IF INIT-IDRT-KEY = 'CDC' OR 'NL1' OR 'FR1' OR 'ES1' OR 'IT1'         
001442                              OR 'WI1' OR 'FI1' OR 'DE1' OR 'ET '         
001443                              OR 'SE1' OR 'SE2' OR 'NO1' OR 'NL3'         
001444                              OR 'GB3' OR 'GB5' OR 'GB7'                  
001445                              OR 'UK2' OR 'DE2'                           
001446                              OR 'DE3' OR 'IT2' OR 'D10' OR 'D11'         
001447                              OR 'NO2' OR 'NL4' OR 'FI2' OR 'FR2'         
001448                              OR 'US1' OR 'US3' OR 'US4' OR 'US5'         
001449                              OR 'US6' OR 'US7' OR 'CA1'                  
001450                              OR 'ET2' OR 'ET3'                           
001451                              OR 'JP1' OR 'JP2' OR 'AU1' OR 'NL5'         
001452                              OR 'DK1' OR 'DK2' OR 'SE3' OR 'SE4'         
001453                              OR 'DE4' OR 'DE5' OR 'DE7' OR 'AE1'         
001454                              OR 'DE8' OR 'DE9' OR 'IT3' OR 'BE1'         
001455                              OR 'CH1' OR 'CN1' OR 'CN2' OR 'CN3'         
001456                              OR 'CN4' OR 'PL1' OR 'IN1' OR 'MY1'         
001457                              OR 'TH1' OR 'TW1' OR 'KR1' OR 'RU1'         
001458                              OR 'CL1' OR 'CL2' OR 'CL3' OR 'CL4'         
001459                              OR 'CL5' OR 'CL6' OR 'CL7' OR 'CL8'         
001460                              OR 'BR1' OR 'MX1' OR 'ZA1' OR 'TR1'         
001461       CONTINUE                                                           
001462     ELSE                                                                 
001463       MOVE INIT-IDDC          TO WS-IDDC                                 
001464       EVALUATE TRUE                                                      
001465         WHEN CDC-SE     MOVE 'CDC' TO INIT-IDRT-KEY                      
001466         WHEN CDC-TR     MOVE 'NL1' TO INIT-IDRT-KEY                      
001467         WHEN SDC-NL     MOVE 'NL1' TO INIT-IDRT-KEY                      
001468         WHEN SDC-NL-ET  MOVE 'ET ' TO INIT-IDRT-KEY                      
001469         WHEN SDC-ES     MOVE 'ES1' TO INIT-IDRT-KEY                      
001470         WHEN SDC-IT     MOVE 'IT1' TO INIT-IDRT-KEY                      
001471         WHEN SDC-AT     MOVE 'WI1' TO INIT-IDRT-KEY                      
001472         WHEN LDC-GB-3A  MOVE 'GB3' TO INIT-IDRT-KEY                      
001473         WHEN LDC-GB-2C  MOVE 'GB5' TO INIT-IDRT-KEY                      
001474         WHEN LDC-GB-3B  MOVE 'GB7' TO INIT-IDRT-KEY                      
001475         WHEN LDC-GB-2H  MOVE 'UK2' TO INIT-IDRT-KEY                      
001476         WHEN LDC-DE-3C  MOVE 'DE2' TO INIT-IDRT-KEY                      
001477         WHEN LDC-DE-2I  MOVE 'DE3' TO INIT-IDRT-KEY                      
001478         WHEN LDC-IT-3F  MOVE 'IT3' TO INIT-IDRT-KEY                      
001479         WHEN LDC-DE-3G  MOVE 'DE7' TO INIT-IDRT-KEY                      
001480         WHEN LDC-DE-3K  MOVE 'DE9' TO INIT-IDRT-KEY                      
001481         WHEN LDC-BE-3L  MOVE 'BE1' TO INIT-IDRT-KEY                      
001482         WHEN LDC-DE-3M  MOVE 'D10' TO INIT-IDRT-KEY                      
001483         WHEN LDC-DE-2L  MOVE 'DE8' TO INIT-IDRT-KEY                      
001484         WHEN LDC-NL-2M  MOVE 'NL3' TO INIT-IDRT-KEY                      
001485         WHEN LDC-IT-3D  MOVE 'IT2' TO INIT-IDRT-KEY                      
001486         WHEN LDC-CH-3H  MOVE 'CH1' TO INIT-IDRT-KEY                      
001487         WHEN LDC-DE-3E  MOVE 'DE4' TO INIT-IDRT-KEY                      
001488         WHEN LDC-DE-2J  MOVE 'DE5' TO INIT-IDRT-KEY                      
001489         WHEN LDC-NL-3N  MOVE 'NL4' TO INIT-IDRT-KEY                      
001490         WHEN LDC-FI-3O  MOVE 'FI2' TO INIT-IDRT-KEY                      
001491         WHEN LDC-FR-3P  MOVE 'FR2' TO INIT-IDRT-KEY                      
001492         WHEN LDC-NL-3R  MOVE 'NL5' TO INIT-IDRT-KEY                      
001493         WHEN LDC-PL-3S  MOVE 'PL1' TO INIT-IDRT-KEY                      
001494         WHEN LDC-DE-3T  MOVE 'D11' TO INIT-IDRT-KEY                      
001495         WHEN LDC-NO-3J  MOVE 'NO2' TO INIT-IDRT-KEY                      
001496         WHEN LDC-CH-2O  MOVE 'CH?' TO INIT-IDRT-KEY                      
001497         WHEN LDC-SE-1C  MOVE 'SE3' TO INIT-IDRT-KEY                      
001498         WHEN LDC-SE-1D  MOVE 'SE4' TO INIT-IDRT-KEY                      
001499         WHEN LDC-SE-1E  MOVE 'SE5' TO INIT-IDRT-KEY                      
001500         WHEN NDC-US-RU  MOVE 'US1' TO INIT-IDRT-KEY                      
001501         WHEN NDC-US-LA  MOVE 'US3' TO INIT-IDRT-KEY                      
001502         WHEN NDC-US-SE  MOVE 'US4' TO INIT-IDRT-KEY                      
001503         WHEN NDC-US-CH  MOVE 'US5' TO INIT-IDRT-KEY                      
001504         WHEN NDC-US-JA  MOVE 'US6' TO INIT-IDRT-KEY                      
001505         WHEN NDC-US-DA  MOVE 'US7' TO INIT-IDRT-KEY                      
001506         WHEN NDC-US-BAT MOVE 'ET2' TO INIT-IDRT-KEY                      
001507         WHEN NDC-TH-93  MOVE 'ET3' TO INIT-IDRT-KEY                      
001508         WHEN NDC-CA     MOVE 'CA1' TO INIT-IDRT-KEY                      
001509         WHEN NDC-JP     MOVE 'JP1' TO INIT-IDRT-KEY                      
001510         WHEN NDC-JP-6A  MOVE 'JP2' TO INIT-IDRT-KEY                      
001511         WHEN NDC-AU     MOVE 'AU1' TO INIT-IDRT-KEY                      
001512         WHEN NDC-AE     MOVE 'AE1' TO INIT-IDRT-KEY                      
001513         WHEN NDC-BR     MOVE 'BR1' TO INIT-IDRT-KEY                      
001514         WHEN NDC-MX     MOVE 'MX1' TO INIT-IDRT-KEY                      
001515         WHEN NDC-ZA     MOVE 'ZA1' TO INIT-IDRT-KEY                      
001516         WHEN NDC-TR     MOVE 'TR1' TO INIT-IDRT-KEY                      
001517         WHEN NDC-IN     MOVE 'IN1' TO INIT-IDRT-KEY                      
001518         WHEN NDC-TH     MOVE 'TH1' TO INIT-IDRT-KEY                      
001519         WHEN NDC-TW     MOVE 'TW1' TO INIT-IDRT-KEY                      
001520         WHEN NDC-KR     MOVE 'KR1' TO INIT-IDRT-KEY                      
001521         WHEN NDC-MY     MOVE 'MY1' TO INIT-IDRT-KEY                      
001522         WHEN NDC-RU-81  MOVE 'RU1' TO INIT-IDRT-KEY                      
001523         WHEN NDC-CN-71  MOVE 'CN1' TO INIT-IDRT-KEY                      
001524         WHEN NDC-CN-72  MOVE 'CN2' TO INIT-IDRT-KEY                      
001525         WHEN NDC-CN-73  MOVE 'CN3' TO INIT-IDRT-KEY                      
001526         WHEN NDC-CN-74  MOVE 'CN4' TO INIT-IDRT-KEY                      
001527         WHEN LDC-CN-7A  MOVE 'CL1' TO INIT-IDRT-KEY                      
001528         WHEN LDC-CN-7B  MOVE 'CL2' TO INIT-IDRT-KEY                      
001529         WHEN LDC-CN-7C  MOVE 'CL3' TO INIT-IDRT-KEY                      
001530         WHEN LDC-CN-7D  MOVE 'CL4' TO INIT-IDRT-KEY                      
001531         WHEN LDC-CN-7E  MOVE 'CL5' TO INIT-IDRT-KEY                      
001532         WHEN LDC-CN-7F  MOVE 'CL6' TO INIT-IDRT-KEY                      
001533         WHEN LDC-CN-7G  MOVE 'CL7' TO INIT-IDRT-KEY                      
001534         WHEN LDC-CN-7H  MOVE 'CL8' TO INIT-IDRT-KEY                      
001535         WHEN OTHER      MOVE 'CDC' TO INIT-IDRT-KEY                      
001536       END-EVALUATE                                                       
001537     END-IF                                                               
001538     IF INIT-IDTIDZON NOT NUMERIC                                         
001539       MOVE '11' TO INIT-IDTIDZON                                         
001540     END-IF                                                               
001541     IF INIT-IDFTG NOT NUMERIC OR INIT-IDFTG = ZERO                       
001542       PERFORM AAA-INIT-IDFTG                                             
001543     END-IF                                                               
001544     IF INIT-KDMATT = 'S' OR 'U'                                          
001545       CONTINUE                                                           
001546     ELSE                                                                 
001547       PERFORM AAG-INIT-KDMATT                                            
001548     END-IF                                                               
001549     IF INIT-KDARBTYP-SEC < 'Å'                                           
001550       MOVE SPACE  TO INIT-KDARBTYP-SEC                                   
001551                      INIT-RESERV-SEC                                     
001552     END-IF                                                               
001553     IF INIT-KDARBTYP-SEC-IDLEV < 'Å'                                     
001554       MOVE SPACE  TO INIT-KDARBTYP-SEC-IDLEV                             
001555                      INIT-RESERV-SEC                                     
001556     END-IF                                                               
001557     IF INIT-KDARBTYP-SEC-4352 < 'Å'                                      
001558       MOVE SPACE  TO INIT-KDARBTYP-SEC-4352                              
001559                      INIT-RESERV-SEC                                     
001560     END-IF                                                               
001561     .                                                                    
001562     EJECT                                                                
001563 B-USER-BEH     SECTION.                                                  
001564                                                                          
001565     MOVE INIT-BEANST            TO MSGI-BEANST                           
001566     MOVE INIT-IDAVD             TO MSGI-IDAVD                            
001567     MOVE INIT-IDCSS             TO MSGI-IDCSS                            
001568     MOVE INIT-IDDC              TO MSGI-IDDC                             
001569     MOVE INIT-IDFTG             TO MSGI-IDFTG                            
001570     MOVE INIT-IDLAND-SPR        TO MSGI-IDLAND-SPR                       
001571     MOVE INIT-IDLTERM           TO MSGI-IDLTERM                          
001572     MOVE INIT-IDNODE            TO MSGI-IDNODE                           
001573     MOVE INIT-IDRT-KEY          TO MSGI-IDRT-KEY                         
001574     MOVE INIT-IDSPRAK           TO MSGI-IDSPRAK                          
001575     MOVE INIT-IDTIDZON          TO MSGI-IDTIDZON                         
001576     MOVE INIT-IDTFN             TO MSGI-IDTFN                            
001577     MOVE INIT-IDTFX             TO MSGI-IDTFX                            
001578     MOVE INIT-IDTRANS           TO MSGI-IDTRANS-OLD                      
001579     MOVE MSGI-IDTRANS           TO INIT-IDTRANS                          
001580     MOVE INIT-KDMATT            TO MSGI-KDMATT                           
001581     MOVE INIT-KDMFSFOR          TO MSGI-KDMFSFOR                         
001582     MOVE SPACE                  TO MSGI-KDSVAR                           
001583     MOVE INIT-SPAR-AREA         TO MSGI-SPAR-AREA                        
001584     .                                                                    
001585     EJECT                                                                
001586 C-KOLLA-KEY    SECTION.                                                  
001587                                                                          
001588     IF MSGI-ADLAGOMR              = ALL '+'                              
001589       MOVE INIT-ADLAGOMR          TO MSGI-ADLAGOMR                       
001590     ELSE                                                                 
001591       MOVE MSGI-ADLAGOMR          TO INIT-ADLAGOMR                       
001592     END-IF                                                               
001593                                                                          
001594     IF MSGI-ADLAGOMR-FOM          = ALL '+'                              
001595       MOVE INIT-ADLAGOMR-FOM      TO MSGI-ADLAGOMR-FOM                   
001596     ELSE                                                                 
001597       MOVE MSGI-ADLAGOMR-FOM      TO INIT-ADLAGOMR-FOM                   
001598     END-IF                                                               
001599                                                                          
001600     IF MSGI-ADLAGOMR-TOM          = ALL '+'                              
001601       MOVE INIT-ADLAGOMR-TOM      TO MSGI-ADLAGOMR-TOM                   
001602     ELSE                                                                 
001603       MOVE MSGI-ADLAGOMR-TOM      TO INIT-ADLAGOMR-TOM                   
001604     END-IF                                                               
001605                                                                          
001606     IF MSGI-ADGANG-FOM            = ALL '+'                              
001607       MOVE INIT-ADGANG-FOM        TO MSGI-ADGANG-FOM                     
001608     ELSE                                                                 
001609       MOVE MSGI-ADGANG-FOM        TO INIT-ADGANG-FOM                     
001610     END-IF                                                               
001611                                                                          
001612     IF MSGI-ADGANG-TOM            = ALL '+'                              
001613       MOVE INIT-ADGANG-TOM        TO MSGI-ADGANG-TOM                     
001614     ELSE                                                                 
001615       MOVE MSGI-ADGANG-TOM        TO INIT-ADGANG-TOM                     
001616     END-IF                                                               
001617                                                                          
001618     IF MSGI-ADLEV-FOM             = ALL '+'                              
001619       MOVE INIT-ADLEV-FOM         TO MSGI-ADLEV-FOM                      
001620     ELSE                                                                 
001621       MOVE MSGI-ADLEV-FOM         TO INIT-ADLEV-FOM                      
001622     END-IF                                                               
001623                                                                          
001624     IF MSGI-ADLEV-TOM             = ALL '+'                              
001625       MOVE INIT-ADLEV-TOM         TO MSGI-ADLEV-TOM                      
001626     ELSE                                                                 
001627       MOVE MSGI-ADLEV-TOM         TO INIT-ADLEV-TOM                      
001628     END-IF                                                               
001629                                                                          
001630     IF MSGI-ADSEC-FOM             = ALL '+'                              
001631       MOVE INIT-ADSEC-FOM         TO MSGI-ADSEC-FOM                      
001632     ELSE                                                                 
001633       MOVE MSGI-ADSEC-FOM         TO INIT-ADSEC-FOM                      
001634     END-IF                                                               
001635                                                                          
001636     IF MSGI-ADSEC-TOM             = ALL '+'                              
001637       MOVE INIT-ADSEC-TOM         TO MSGI-ADSEC-TOM                      
001638     ELSE                                                                 
001639       MOVE MSGI-ADSEC-TOM         TO INIT-ADSEC-TOM                      
001640     END-IF                                                               
001641                                                                          
001642     IF MSGI-ADSEQ-FOM             = ALL '+'                              
001643       MOVE INIT-ADSEQ-FOM         TO MSGI-ADSEQ-FOM                      
001644     ELSE                                                                 
001645       MOVE MSGI-ADSEQ-FOM         TO INIT-ADSEQ-FOM                      
001646     END-IF                                                               
001647                                                                          
001648     IF MSGI-ADSEQ-TOM             = ALL '+'                              
001649       MOVE INIT-ADSEQ-TOM         TO MSGI-ADSEQ-TOM                      
001650     ELSE                                                                 
001651       MOVE MSGI-ADSEQ-TOM         TO INIT-ADSEQ-TOM                      
001652     END-IF                                                               
001653                                                                          
001654     IF MSGI-ADPLATS               = ALL '+'                              
001655       MOVE INIT-ADPLATS           TO MSGI-ADPLATS                        
001656     ELSE                                                                 
001657       MOVE MSGI-ADPLATS           TO INIT-ADPLATS                        
001658     END-IF                                                               
001659                                                                          
001660     IF MSGI-ADINLOMR              = ALL '+'                              
001661       MOVE INIT-ADINLOMR          TO MSGI-ADINLOMR                       
001662     ELSE                                                                 
001663       MOVE MSGI-ADINLOMR          TO INIT-ADINLOMR                       
001664     END-IF                                                               
001665                                                                          
001666     IF MSGI-ADINLOMR-NXT          = ALL '+'                              
001667       MOVE INIT-ADINLOMR-NXT      TO MSGI-ADINLOMR-NXT                   
001668     ELSE                                                                 
001669       MOVE MSGI-ADINLOMR-NXT      TO INIT-ADINLOMR-NXT                   
001670     END-IF                                                               
001671                                                                          
001672     IF MSGI-ADINLOMR-PRT          = ALL '+'                              
001673       MOVE INIT-ADINLOMR-PRT      TO MSGI-ADINLOMR-PRT                   
001674     ELSE                                                                 
001675       MOVE MSGI-ADINLOMR-PRT      TO INIT-ADINLOMR-PRT                   
001676     END-IF                                                               
001677                                                                          
001678     IF MSGI-BEFT                  = ALL '+'                              
001679       MOVE INIT-BEFT              TO MSGI-BEFT                           
001680     ELSE                                                                 
001681       MOVE MSGI-BEFT              TO INIT-BEFT                           
001682     END-IF                                                               
001683                                                                          
001684     IF MSGI-BEWEBSCR              = ALL '+'                              
001685       MOVE INIT-BEWEBSCR          TO MSGI-BEWEBSCR                       
001686     ELSE                                                                 
001687       MOVE MSGI-BEWEBSCR          TO INIT-BEWEBSCR                       
001688     END-IF                                                               
001689                                                                          
001690     IF MSGI-DASUPREF              = ALL '+'                              
001691       MOVE INIT-DASUPREF          TO MSGI-DASUPREF                       
001692     ELSE                                                                 
001693       MOVE MSGI-DASUPREF          TO INIT-DASUPREF                       
001694     END-IF                                                               
001695                                                                          
001696     IF MSGI-FLINLFB               = ALL '+'                              
001697       MOVE INIT-FLINLFB           TO MSGI-FLINLFB                        
001698     ELSE                                                                 
001699       MOVE MSGI-FLINLFB           TO INIT-FLINLFB                        
001700     END-IF                                                               
001701                                                                          
001702     IF MSGI-FLINLI                = ALL '+'                              
001703       MOVE INIT-FLINLI            TO MSGI-FLINLI                         
001704     ELSE                                                                 
001705       MOVE MSGI-FLINLI            TO INIT-FLINLI                         
001706     END-IF                                                               
001707                                                                          
001708     IF MSGI-FLLDCKND              = ALL '+'                              
001709       MOVE INIT-FLLDCKND          TO MSGI-FLLDCKND                       
001710     ELSE                                                                 
001711       MOVE MSGI-FLLDCKND          TO INIT-FLLDCKND                       
001712     END-IF                                                               
001713                                                                          
001714     IF MSGI-FLMRKVAL              = ALL '+'                              
001715       MOVE INIT-FLMRKVAL          TO MSGI-FLMRKVAL                       
001716     ELSE                                                                 
001717       MOVE MSGI-FLMRKVAL          TO INIT-FLMRKVAL                       
001718     END-IF                                                               
001719                                                                          
001720     IF MSGI-FLORDLEV              = ALL '+'                              
001721       MOVE INIT-FLORDLEV          TO MSGI-FLORDLEV                       
001722     ELSE                                                                 
001723       MOVE MSGI-FLORDLEV          TO INIT-FLORDLEV                       
001724     END-IF                                                               
001725                                                                          
001726     IF MSGI-FLVISA                = ALL '+'                              
001727       MOVE INIT-FLVISA            TO MSGI-FLVISA                         
001728     ELSE                                                                 
001729       MOVE MSGI-FLVISA            TO INIT-FLVISA                         
001730     END-IF                                                               
001731                                                                          
001732     IF MSGI-FLSORT                = ALL '+'                              
001733       MOVE INIT-FLSORT            TO MSGI-FLSORT                         
001734     ELSE                                                                 
001735       MOVE MSGI-FLSORT            TO INIT-FLSORT                         
001736     END-IF                                                               
001737                                                                          
001738     IF MSGI-IDANSK                = ALL '+'                              
001739       MOVE INIT-IDANSK            TO MSGI-IDANSK                         
001740     ELSE                                                                 
001741       MOVE MSGI-IDANSK            TO INIT-IDANSK                         
001742     END-IF                                                               
001743                                                                          
001744     IF MSGI-IDANSK-FOM            = ALL '+'                              
001745       MOVE INIT-IDANSK-FOM        TO MSGI-IDANSK-FOM                     
001746     ELSE                                                                 
001747       MOVE MSGI-IDANSK-FOM        TO INIT-IDANSK-FOM                     
001748     END-IF                                                               
001749                                                                          
001750     IF MSGI-IDANSK-TOM            = ALL '+'                              
001751       MOVE INIT-IDANSK-TOM        TO MSGI-IDANSK-TOM                     
001752     ELSE                                                                 
001753       MOVE MSGI-IDANSK-TOM        TO INIT-IDANSK-TOM                     
001754     END-IF                                                               
001755                                                                          
001756     IF MSGI-IDANSTNR              = ALL '+'                              
001757       MOVE INIT-IDANSTNR          TO MSGI-IDANSTNR                       
001758     ELSE                                                                 
001759       MOVE MSGI-IDANSTNR          TO INIT-IDANSTNR                       
001760     END-IF                                                               
001761                                                                          
001762     IF MSGI-IDARTNR               = ALL '+'                              
001763       MOVE INIT-IDARTNR           TO MSGI-IDARTNR                        
001764     ELSE                                                                 
001765       MOVE MSGI-IDARTNR           TO INIT-IDARTNR                        
001766     END-IF                                                               
001767                                                                          
001768     IF MSGI-IDBYTKOL              = ALL '+'                              
001769       MOVE INIT-IDBYTKOL          TO MSGI-IDBYTKOL                       
001770     ELSE                                                                 
001771       MOVE MSGI-IDBYTKOL          TO INIT-IDBYTKOL                       
001772     END-IF                                                               
001773                                                                          
001774     IF MSGI-IDBYTRAP              = ALL '+'                              
001775       MOVE INIT-IDBYTRAP          TO MSGI-IDBYTRAP                       
001776     ELSE                                                                 
001777       MOVE MSGI-IDBYTRAP          TO INIT-IDBYTRAP                       
001778     END-IF                                                               
001779                                                                          
001780     IF MSGI-IDCATAVS              = ALL '+'                              
001781       MOVE INIT-IDCATAVS          TO MSGI-IDCATAVS                       
001782     ELSE                                                                 
001783       MOVE MSGI-IDCATAVS          TO INIT-IDCATAVS                       
001784     END-IF                                                               
001785                                                                          
001786     IF MSGI-IDCATGRP              = ALL '+'                              
001787       MOVE INIT-IDCATGRP          TO MSGI-IDCATGRP                       
001788     ELSE                                                                 
001789       MOVE MSGI-IDCATGRP          TO INIT-IDCATGRP                       
001790     END-IF                                                               
001791                                                                          
001792     IF MSGI-IDCATNR               = ALL '+'                              
001793       MOVE INIT-IDCATNR           TO MSGI-IDCATNR                        
001794     ELSE                                                                 
001795       MOVE MSGI-IDCATNR           TO INIT-IDCATNR                        
001796     END-IF                                                               
001797                                                                          
001798     IF MSGI-IDCATRAD              = ALL '+'                              
001799       MOVE INIT-IDCATRAD          TO MSGI-IDCATRAD                       
001800     ELSE                                                                 
001801       MOVE MSGI-IDCATRAD          TO INIT-IDCATRAD                       
001802     END-IF                                                               
001803                                                                          
001804     IF MSGI-IDDC-BULK             = ALL '+'                              
001805       MOVE INIT-IDDC-BULK         TO MSGI-IDDC-BULK                      
001806     ELSE                                                                 
001807       MOVE MSGI-IDDC-BULK         TO INIT-IDDC-BULK                      
001808     END-IF                                                               
001809                                                                          
001810     IF MSGI-IDDC-DAY              = ALL '+'                              
001811       MOVE INIT-IDDC-DAY          TO MSGI-IDDC-DAY                       
001812     ELSE                                                                 
001813       MOVE MSGI-IDDC-DAY          TO INIT-IDDC-DAY                       
001814     END-IF                                                               
001815                                                                          
001816     IF MSGI-IDDC-KEY              = ALL '+'                              
001817       MOVE INIT-IDDC-KEY          TO MSGI-IDDC-KEY                       
001818     ELSE                                                                 
001819       MOVE MSGI-IDDC-KEY          TO INIT-IDDC-KEY                       
001820     END-IF                                                               
001821                                                                          
001822     IF MSGI-IDDC-REC              = ALL '+'                              
001823       MOVE INIT-IDDC-REC          TO MSGI-IDDC-REC                       
001824     ELSE                                                                 
001825       MOVE MSGI-IDDC-REC          TO INIT-IDDC-REC                       
001826     END-IF                                                               
001827                                                                          
001828     IF MSGI-IDDC-REF              = ALL '+'                              
001829       MOVE INIT-IDDC-REF          TO MSGI-IDDC-REF                       
001830     ELSE                                                                 
001831       MOVE MSGI-IDDC-REF          TO INIT-IDDC-REF                       
001832     END-IF                                                               
001833                                                                          
001834     IF MSGI-IDDC-SEND             = ALL '+'                              
001835       MOVE INIT-IDDC-SEND         TO MSGI-IDDC-SEND                      
001836     ELSE                                                                 
001837       MOVE MSGI-IDDC-SEND         TO INIT-IDDC-SEND                      
001838     END-IF                                                               
001839                                                                          
001840     IF MSGI-IDDIRGRP              = ALL '+'                              
001841       MOVE INIT-IDDIRGRP          TO MSGI-IDDIRGRP                       
001842     ELSE                                                                 
001843       MOVE MSGI-IDDIRGRP          TO INIT-IDDIRGRP                       
001844     END-IF                                                               
001845                                                                          
001846     IF MSGI-IDDISTR               = ALL '+'                              
001847       MOVE INIT-IDDISTR           TO MSGI-IDDISTR                        
001848     ELSE                                                                 
001849       MOVE MSGI-IDDISTR           TO INIT-IDDISTR                        
001850     END-IF                                                               
001851                                                                          
001852     IF MSGI-IDDISTR-FOM           = ALL '+'                              
001853       MOVE INIT-IDDISTR-FOM       TO MSGI-IDDISTR-FOM                    
001854     ELSE                                                                 
001855       MOVE MSGI-IDDISTR-FOM       TO INIT-IDDISTR-FOM                    
001856     END-IF                                                               
001857                                                                          
001858     IF MSGI-IDDISTR-TOM           = ALL '+'                              
001859       MOVE INIT-IDDISTR-TOM       TO MSGI-IDDISTR-TOM                    
001860     ELSE                                                                 
001861       MOVE MSGI-IDDISTR-TOM       TO INIT-IDDISTR-TOM                    
001862     END-IF                                                               
001863                                                                          
001864     IF MSGI-IDFAKT                = ALL '+'                              
001865       MOVE INIT-IDFAKT            TO MSGI-IDFAKT                         
001866     ELSE                                                                 
001867       MOVE MSGI-IDFAKT            TO INIT-IDFAKT                         
001868     END-IF                                                               
001869                                                                          
001870     IF MSGI-IDFKNGRP              = ALL '+'                              
001871       MOVE INIT-IDFKNGRP          TO MSGI-IDFKNGRP                       
001872     ELSE                                                                 
001873       MOVE MSGI-IDFKNGRP          TO INIT-IDFKNGRP                       
001874     END-IF                                                               
001875                                                                          
001876     IF MSGI-IDFOTNR               = ALL '+'                              
001877       MOVE INIT-IDFOTNR           TO MSGI-IDFOTNR                        
001878     ELSE                                                                 
001879       MOVE MSGI-IDFOTNR           TO INIT-IDFOTNR                        
001880     END-IF                                                               
001881                                                                          
001882     IF MSGI-IDFPINST              = ALL '+'                              
001883       MOVE INIT-IDFPINST          TO MSGI-IDFPINST                       
001884     ELSE                                                                 
001885       MOVE MSGI-IDFPINST          TO INIT-IDFPINST                       
001886     END-IF                                                               
001887                                                                          
001888     IF MSGI-IDFS                  = ALL '+'                              
001889       MOVE INIT-IDFS              TO MSGI-IDFS                           
001890     ELSE                                                                 
001891       MOVE MSGI-IDFS              TO INIT-IDFS                           
001892     END-IF                                                               
001893                                                                          
001894     IF MSGI-IDFTG-KEY             = ALL '+'                              
001895       MOVE INIT-IDFTG-KEY         TO MSGI-IDFTG-KEY                      
001896     ELSE                                                                 
001897       MOVE MSGI-IDFTG-KEY         TO INIT-IDFTG-KEY                      
001898     END-IF                                                               
001899                                                                          
001900     IF MSGI-IDILIRAD              = ALL '+'                              
001901       MOVE INIT-IDILIRAD          TO MSGI-IDILIRAD                       
001902     ELSE                                                                 
001903       MOVE MSGI-IDILIRAD          TO INIT-IDILIRAD                       
001904     END-IF                                                               
001905                                                                          
001906     IF MSGI-IDILIST               = ALL '+'                              
001907       MOVE INIT-IDILIST           TO MSGI-IDILIST                        
001908     ELSE                                                                 
001909       MOVE MSGI-IDILIST           TO INIT-IDILIST                        
001910     END-IF                                                               
001911                                                                          
001912     IF MSGI-IDILLU                = ALL '+'                              
001913       MOVE INIT-IDILLU            TO MSGI-IDILLU                         
001914     ELSE                                                                 
001915       MOVE MSGI-IDILLU            TO INIT-IDILLU                         
001916     END-IF                                                               
001917                                                                          
001918     IF MSGI-IDINLVGN              = ALL '+'                              
001919       MOVE INIT-IDINLVGN          TO MSGI-IDINLVGN                       
001920     ELSE                                                                 
001921       MOVE MSGI-IDINLVGN          TO INIT-IDINLVGN                       
001922     END-IF                                                               
001923                                                                          
001924     IF MSGI-IDKAMP                = ALL '+'                              
001925       MOVE INIT-IDKAMP            TO MSGI-IDKAMP                         
001926     ELSE                                                                 
001927       MOVE MSGI-IDKAMP            TO INIT-IDKAMP                         
001928     END-IF                                                               
001929                                                                          
001930     IF MSGI-IDKAMP-GRP            = ALL '+'                              
001931       MOVE INIT-IDKAMP-GRP        TO MSGI-IDKAMP-GRP                     
001932     ELSE                                                                 
001933       MOVE MSGI-IDKAMP-GRP        TO INIT-IDKAMP-GRP                     
001934     END-IF                                                               
001935                                                                          
001936     IF MSGI-IDKAMPRF              = ALL '+'                              
001937       MOVE INIT-IDKAMPRF          TO MSGI-IDKAMPRF                       
001938     ELSE                                                                 
001939       MOVE MSGI-IDKAMPRF          TO INIT-IDKAMPRF                       
001940     END-IF                                                               
001941                                                                          
001942     IF MSGI-IDKOLLI               = ALL '+'                              
001943       MOVE INIT-IDKOLLI           TO MSGI-IDKOLLI                        
001944     ELSE                                                                 
001945       MOVE MSGI-IDKOLLI           TO INIT-IDKOLLI                        
001946     END-IF                                                               
001947                                                                          
001948     IF MSGI-IDKOLLI-SAMP          = ALL '+'                              
001949       MOVE INIT-IDKOLLI-SAMP      TO MSGI-IDKOLLI-SAMP                   
001950     ELSE                                                                 
001951       MOVE MSGI-IDKOLLI-SAMP      TO INIT-IDKOLLI-SAMP                   
001952     END-IF                                                               
001953                                                                          
001954     IF MSGI-IDKONCNR              = ALL '+'                              
001955       MOVE INIT-IDKONCNR          TO MSGI-IDKONCNR                       
001956     ELSE                                                                 
001957       MOVE MSGI-IDKONCNR          TO INIT-IDKONCNR                       
001958     END-IF                                                               
001959                                                                          
001960     IF MSGI-IDKONTO               = ALL '+'                              
001961       MOVE INIT-IDKONTO           TO MSGI-IDKONTO                        
001962     ELSE                                                                 
001963       MOVE MSGI-IDKONTO           TO INIT-IDKONTO                        
001964     END-IF                                                               
001965                                                                          
001966     IF MSGI-IDKR                  = ALL '+'                              
001967       MOVE INIT-IDKR              TO MSGI-IDKR                           
001968     ELSE                                                                 
001969       MOVE MSGI-IDKR              TO INIT-IDKR                           
001970     END-IF                                                               
001971                                                                          
001972     IF MSGI-IDKUNDNR              = ALL '+'                              
001973       MOVE INIT-IDKUNDNR          TO MSGI-IDKUNDNR                       
001974     ELSE                                                                 
001975       MOVE MSGI-IDKUNDNR          TO INIT-IDKUNDNR                       
001976     END-IF                                                               
001977                                                                          
001978     IF MSGI-IDKUNDRF              = ALL '+'                              
001979       MOVE INIT-IDKUNDRF          TO MSGI-IDKUNDRF                       
001980     ELSE                                                                 
001981       MOVE MSGI-IDKUNDRF          TO INIT-IDKUNDRF                       
001982     END-IF                                                               
001983                                                                          
001984     IF MSGI-IDLANDX2              = ALL '+'                              
001985       MOVE INIT-IDLANDX2          TO MSGI-IDLANDX2                       
001986     ELSE                                                                 
001987       MOVE MSGI-IDLANDX2          TO INIT-IDLANDX2                       
001988     END-IF                                                               
001989                                                                          
001990     IF MSGI-IDLASTN               = ALL '+'                              
001991       MOVE INIT-IDLASTN           TO MSGI-IDLASTN                        
001992     ELSE                                                                 
001993       MOVE MSGI-IDLASTN           TO INIT-IDLASTN                        
001994     END-IF                                                               
001995                                                                          
001996     IF MSGI-IDLBBET               = ALL '+'                              
001997       MOVE INIT-IDLBBET           TO MSGI-IDLBBET                        
001998     ELSE                                                                 
001999       MOVE MSGI-IDLBBET           TO INIT-IDLBBET                        
002000     END-IF                                                               
002001                                                                          
002002     IF MSGI-IDLBFIKT              = ALL '+'                              
002003       MOVE INIT-IDLBFIKT          TO MSGI-IDLBFIKT                       
002004     ELSE                                                                 
002005       MOVE MSGI-IDLBFIKT          TO INIT-IDLBFIKT                       
002006     END-IF                                                               
002007                                                                          
002008     IF MSGI-IDLEVNR               = ALL '+'                              
002009       MOVE INIT-IDLEVNR           TO MSGI-IDLEVNR                        
002010     ELSE                                                                 
002011       MOVE MSGI-IDLEVNR           TO INIT-IDLEVNR                        
002012     END-IF                                                               
002013                                                                          
002014     IF MSGI-IDLOPNRM              = ALL '+'                              
002015       MOVE INIT-IDLOPNRM          TO MSGI-IDLOPNRM                       
002016     ELSE                                                                 
002017       MOVE MSGI-IDLOPNRM          TO INIT-IDLOPNRM                       
002018     END-IF                                                               
002019                                                                          
002020     IF MSGI-IDSPRAK-KEY           = ALL '+'                              
002021       MOVE INIT-IDSPRAK-KEY       TO MSGI-IDSPRAK-KEY                    
002022     ELSE                                                                 
002023       MOVE MSGI-IDSPRAK-KEY       TO INIT-IDSPRAK-KEY                    
002024     END-IF                                                               
002025                                                                          
002026     IF MSGI-IDMARKBO              = ALL '+'                              
002027       MOVE INIT-IDMARKBO          TO MSGI-IDMARKBO                       
002028     ELSE                                                                 
002029       MOVE MSGI-IDMARKBO          TO INIT-IDMARKBO                       
002030     END-IF                                                               
002031                                                                          
002032     IF MSGI-IDOKOLLI              = ALL '+'                              
002033       MOVE INIT-IDOKOLLI          TO MSGI-IDOKOLLI                       
002034     ELSE                                                                 
002035       MOVE MSGI-IDOKOLLI          TO INIT-IDOKOLLI                       
002036     END-IF                                                               
002037                                                                          
002038     IF MSGI-IDPARTNR              = ALL '+'                              
002039       MOVE INIT-IDPARTNR          TO MSGI-IDPARTNR                       
002040     ELSE                                                                 
002041       MOVE MSGI-IDPARTNR          TO INIT-IDPARTNR                       
002042     END-IF                                                               
002043                                                                          
002044     IF MSGI-IDPERSON              = ALL '+'                              
002045       MOVE INIT-IDPERSON          TO MSGI-IDPERSON                       
002046     ELSE                                                                 
002047       MOVE MSGI-IDPERSON          TO INIT-IDPERSON                       
002048     END-IF                                                               
002049                                                                          
002050     IF MSGI-IDPERSON-CDC          = ALL '+'                              
002051       MOVE INIT-IDPERSON-CDC      TO MSGI-IDPERSON-CDC                   
002052     ELSE                                                                 
002053       MOVE MSGI-IDPERSON-CDC      TO INIT-IDPERSON-CDC                   
002054     END-IF                                                               
002055                                                                          
002056     IF MSGI-IDPERSON-QUAL         = ALL '+'                              
002057       MOVE INIT-IDPERSON-QUAL     TO MSGI-IDPERSON-QUAL                  
002058     ELSE                                                                 
002059       MOVE MSGI-IDPERSON-QUAL     TO INIT-IDPERSON-QUAL                  
002060     END-IF                                                               
002061                                                                          
002062     IF MSGI-IDPRCBAS              = ALL '+'                              
002063       MOVE INIT-IDPRCBAS          TO MSGI-IDPRCBAS                       
002064     ELSE                                                                 
002065       MOVE MSGI-IDPRCBAS          TO INIT-IDPRCBAS                       
002066     END-IF                                                               
002067                                                                          
002068     IF MSGI-IDPRCTAB              = ALL '+'                              
002069       MOVE INIT-IDPRCTAB          TO MSGI-IDPRCTAB                       
002070     ELSE                                                                 
002071       MOVE MSGI-IDPRCTAB          TO INIT-IDPRCTAB                       
002072     END-IF                                                               
002073                                                                          
002074     IF MSGI-IDPRCTR               = ALL '+'                              
002075       MOVE INIT-IDPRCTR           TO MSGI-IDPRCTR                        
002076     ELSE                                                                 
002077       MOVE MSGI-IDPRCTR           TO INIT-IDPRCTR                        
002078     END-IF                                                               
002079                                                                          
002080     IF MSGI-IDPRCVAR              = ALL '+'                              
002081       MOVE INIT-IDPRCVAR          TO MSGI-IDPRCVAR                       
002082     ELSE                                                                 
002083       MOVE MSGI-IDPRCVAR          TO INIT-IDPRCVAR                       
002084     END-IF                                                               
002085                                                                          
002086     IF MSGI-IDPRODNR              = ALL '+'                              
002087       MOVE INIT-IDPRODNR          TO MSGI-IDPRODNR                       
002088     ELSE                                                                 
002089       MOVE MSGI-IDPRODNR          TO INIT-IDPRODNR                       
002090     END-IF                                                               
002091                                                                          
002092     IF MSGI-IDPROJ                = ALL '+'                              
002093       MOVE INIT-IDPROJ            TO MSGI-IDPROJ                         
002094     ELSE                                                                 
002095       MOVE MSGI-IDPROJ            TO INIT-IDPROJ                         
002096     END-IF                                                               
002097                                                                          
002098     IF MSGI-IDPROMRN              = ALL '+'                              
002099       MOVE INIT-IDPROMRN          TO MSGI-IDPROMRN                       
002100     ELSE                                                                 
002101       MOVE MSGI-IDPROMRN          TO INIT-IDPROMRN                       
002102     END-IF                                                               
002103                                                                          
002104     IF MSGI-IDPTYP                = ALL '+'                              
002105       MOVE INIT-IDPTYP            TO MSGI-IDPTYP                         
002106     ELSE                                                                 
002107       MOVE MSGI-IDPTYP            TO INIT-IDPTYP                         
002108     END-IF                                                               
002109                                                                          
002110     IF MSGI-IDRADNR               = ALL '+'                              
002111       MOVE INIT-IDRADNR           TO MSGI-IDRADNR                        
002112     ELSE                                                                 
002113       MOVE MSGI-IDRADNR           TO INIT-IDRADNR                        
002114     END-IF                                                               
002115                                                                          
002116     IF MSGI-IDRAPPNR              = ALL '+'                              
002117       MOVE INIT-IDRAPPNR          TO MSGI-IDRAPPNR                       
002118     ELSE                                                                 
002119       MOVE MSGI-IDRAPPNR          TO INIT-IDRAPPNR                       
002120     END-IF                                                               
002121                                                                          
002122     IF MSGI-IDRAPP                = ALL '+'                              
002123       MOVE INIT-IDRAPP            TO MSGI-IDRAPP                         
002124     ELSE                                                                 
002125       MOVE MSGI-IDRAPP            TO INIT-IDRAPP                         
002126     END-IF                                                               
002127                                                                          
002128     IF MSGI-IDROLL                = ALL '+'                              
002129       MOVE INIT-IDROLL            TO MSGI-IDROLL                         
002130     ELSE                                                                 
002131       MOVE MSGI-IDROLL            TO INIT-IDROLL                         
002132     END-IF                                                               
002133                                                                          
002134     IF MSGI-IDRT                 = ALL '+'                               
002135       MOVE INIT-IDRT              TO MSGI-IDRT                           
002136     ELSE                                                                 
002137       MOVE MSGI-IDRT              TO INIT-IDRT                           
002138     END-IF                                                               
002139                                                                          
002140     IF MSGI-IDRTLOP              = ALL '+'                               
002141       MOVE INIT-IDRTLOP           TO MSGI-IDRTLOP                        
002142     ELSE                                                                 
002143       MOVE MSGI-IDRTLOP           TO INIT-IDRTLOP                        
002144     END-IF                                                               
002145                                                                          
002146     IF MSGI-IDRUBNR              = ALL '+'                               
002147       MOVE INIT-IDRUBNR           TO MSGI-IDRUBNR                        
002148     ELSE                                                                 
002149       MOVE MSGI-IDRUBNR           TO INIT-IDRUBNR                        
002150     END-IF                                                               
002151                                                                          
002152     IF MSGI-IDSHIPM               = ALL '+'                              
002153       MOVE INIT-IDSHIPM           TO MSGI-IDSHIPM                        
002154     ELSE                                                                 
002155       MOVE MSGI-IDSHIPM           TO INIT-IDSHIPM                        
002156     END-IF                                                               
002157                                                                          
002158     IF MSGI-IDSKEPPN              = ALL '+'                              
002159       MOVE INIT-IDSKEPPN          TO MSGI-IDSKEPPN                       
002160     ELSE                                                                 
002161       MOVE MSGI-IDSKEPPN          TO INIT-IDSKEPPN                       
002162     END-IF                                                               
002163                                                                          
002164     IF MSGI-IDSNDJOB              = ALL '+'                              
002165       MOVE INIT-IDSNDJOB          TO MSGI-IDSNDJOB                       
002166     ELSE                                                                 
002167       MOVE MSGI-IDSNDJOB          TO INIT-IDSNDJOB                       
002168     END-IF                                                               
002169                                                                          
002170     IF MSGI-IDSNDNOD              = ALL '+'                              
002171       MOVE INIT-IDSNDNOD          TO MSGI-IDSNDNOD                       
002172     ELSE                                                                 
002173       MOVE MSGI-IDSNDNOD          TO INIT-IDSNDNOD                       
002174     END-IF                                                               
002175                                                                          
002176     IF MSGI-IDSUPREF              = ALL '+'                              
002177       MOVE INIT-IDSUPREF          TO MSGI-IDSUPREF                       
002178     ELSE                                                                 
002179       MOVE MSGI-IDSUPREF          TO INIT-IDSUPREF                       
002180     END-IF                                                               
002181                                                                          
002182     IF MSGI-IDSYSMOT              = ALL '+'                              
002183       MOVE INIT-IDSYSMOT          TO MSGI-IDSYSMOT                       
002184     ELSE                                                                 
002185       MOVE MSGI-IDSYSMOT          TO INIT-IDSYSMOT                       
002186     END-IF                                                               
002187                                                                          
002188     IF MSGI-IDTABNR               = ALL '+'                              
002189       MOVE INIT-IDTABNR           TO MSGI-IDTABNR                        
002190     ELSE                                                                 
002191       MOVE MSGI-IDTABNR           TO INIT-IDTABNR                        
002192     END-IF                                                               
002193                                                                          
002194     IF MSGI-IDTRPTNR              = ALL '+'                              
002195       MOVE INIT-IDTRPTNR          TO MSGI-IDTRPTNR                       
002196     ELSE                                                                 
002197       MOVE MSGI-IDTRPTNR          TO INIT-IDTRPTNR                       
002198     END-IF                                                               
002199                                                                          
002200     IF MSGI-IDTTEXNR              = ALL '+'                              
002201       MOVE INIT-IDTTEXNR          TO MSGI-IDTTEXNR                       
002202     ELSE                                                                 
002203       MOVE MSGI-IDTTEXNR          TO INIT-IDTTEXNR                       
002204     END-IF                                                               
002205                                                                          
002206     IF MSGI-IDUSER-KEY            = ALL '+'                              
002207       MOVE INIT-IDUSER-KEY        TO MSGI-IDUSER-KEY                     
002208     ELSE                                                                 
002209       MOVE MSGI-IDUSER-KEY        TO INIT-IDUSER-KEY                     
002210     END-IF                                                               
002211                                                                          
002212     IF MSGI-IDVO                  = ALL '+'                              
002213       MOVE INIT-IDVO              TO MSGI-IDVO                           
002214     ELSE                                                                 
002215       MOVE MSGI-IDVO              TO INIT-IDVO                           
002216     END-IF                                                               
002217                                                                          
002218     IF MSGI-KDANMORS              = ALL '+'                              
002219       MOVE INIT-KDANMORS          TO MSGI-KDANMORS                       
002220     ELSE                                                                 
002221       MOVE MSGI-KDANMORS          TO INIT-KDANMORS                       
002222     END-IF                                                               
002223                                                                          
002224     IF MSGI-KDARBTYP              = ALL '+'                              
002225       MOVE INIT-KDARBTYP          TO MSGI-KDARBTYP                       
002226     ELSE                                                                 
002227       MOVE MSGI-KDARBTYP          TO INIT-KDARBTYP                       
002228     END-IF                                                               
002229                                                                          
002230     IF MSGI-KDARBVAL              = ALL '+'                              
002231       MOVE INIT-KDARBVAL          TO MSGI-KDARBVAL                       
002232     ELSE                                                                 
002233       MOVE MSGI-KDARBVAL          TO INIT-KDARBVAL                       
002234     END-IF                                                               
002235                                                                          
002236     IF MSGI-KDARTKAM              = ALL '+'                              
002237       MOVE INIT-KDARTKAM          TO MSGI-KDARTKAM                       
002238     ELSE                                                                 
002239       MOVE MSGI-KDARTKAM          TO INIT-KDARTKAM                       
002240     END-IF                                                               
002241                                                                          
002242     IF MSGI-KDAVROP               = ALL '+'                              
002243       MOVE INIT-KDAVROP           TO MSGI-KDAVROP                        
002244     ELSE                                                                 
002245       MOVE MSGI-KDAVROP           TO INIT-KDAVROP                        
002246     END-IF                                                               
002247                                                                          
002248     IF MSGI-KDBYTSTA              = ALL '+'                              
002249       MOVE INIT-KDBYTSTA          TO MSGI-KDBYTSTA                       
002250     ELSE                                                                 
002251       MOVE MSGI-KDBYTSTA          TO INIT-KDBYTSTA                       
002252     END-IF                                                               
002253                                                                          
002254     IF MSGI-KDEKHHT               = ALL '+'                              
002255       MOVE INIT-KDEKHHT           TO MSGI-KDEKHHT                        
002256     ELSE                                                                 
002257       MOVE MSGI-KDEKHHT           TO INIT-KDEKHHT                        
002258     END-IF                                                               
002259                                                                          
002260     IF MSGI-KDEKNIVA              = ALL '+'                              
002261       MOVE INIT-KDEKNIVA          TO MSGI-KDEKNIVA                       
002262     ELSE                                                                 
002263       MOVE MSGI-KDEKNIVA          TO INIT-KDEKNIVA                       
002264     END-IF                                                               
002265                                                                          
002266     IF MSGI-KDEKSHT               = ALL '+'                              
002267       MOVE INIT-KDEKSHT           TO MSGI-KDEKSHT                        
002268     ELSE                                                                 
002269       MOVE MSGI-KDEKSHT           TO INIT-KDEKSHT                        
002270     END-IF                                                               
002271                                                                          
002272     IF MSGI-KDEXCHA               = ALL '+'                              
002273       MOVE INIT-KDEXCHA           TO MSGI-KDEXCHA                        
002274     ELSE                                                                 
002275       MOVE MSGI-KDEXCHA           TO INIT-KDEXCHA                        
002276     END-IF                                                               
002277                                                                          
002278     IF MSGI-KDFARLIG              = ALL '+'                              
002279       MOVE INIT-KDFARLIG          TO MSGI-KDFARLIG                       
002280     ELSE                                                                 
002281       MOVE MSGI-KDFARLIG          TO INIT-KDFARLIG                       
002282     END-IF                                                               
002283                                                                          
002284     IF MSGI-KDFRAKT               = ALL '+'                              
002285       MOVE INIT-KDFRAKT           TO MSGI-KDFRAKT                        
002286     ELSE                                                                 
002287       MOVE MSGI-KDFRAKT           TO INIT-KDFRAKT                        
002288     END-IF                                                               
002289                                                                          
002290     IF MSGI-KDFREQ                = ALL '+'                              
002291       MOVE INIT-KDFREQ            TO MSGI-KDFREQ                         
002292     ELSE                                                                 
002293       MOVE MSGI-KDFREQ            TO INIT-KDFREQ                         
002294     END-IF                                                               
002295                                                                          
002296     IF MSGI-KDKOMSTA              = ALL '+'                              
002297       MOVE INIT-KDKOMSTA          TO MSGI-KDKOMSTA                       
002298     ELSE                                                                 
002299       MOVE MSGI-KDKOMSTA          TO INIT-KDKOMSTA                       
002300     END-IF                                                               
002301                                                                          
002302     IF MSGI-KDKRENOT              = ALL '+'                              
002303       MOVE INIT-KDKRENOT          TO MSGI-KDKRENOT                       
002304     ELSE                                                                 
002305       MOVE MSGI-KDKRENOT          TO INIT-KDKRENOT                       
002306     END-IF                                                               
002307                                                                          
002308     IF MSGI-KDLARM                = ALL '+'                              
002309       MOVE INIT-KDLARM            TO MSGI-KDLARM                         
002310     ELSE                                                                 
002311       MOVE MSGI-KDLARM            TO INIT-KDLARM                         
002312     END-IF                                                               
002313                                                                          
002314     IF MSGI-KDLEVANM-FOM          = ALL '+'                              
002315       MOVE INIT-KDLEVANM-FOM      TO MSGI-KDLEVANM-FOM                   
002316     ELSE                                                                 
002317       MOVE MSGI-KDLEVANM-FOM      TO INIT-KDLEVANM-FOM                   
002318     END-IF                                                               
002319                                                                          
002320     IF MSGI-KDLEVANM-TOM          = ALL '+'                              
002321       MOVE INIT-KDLEVANM-TOM      TO MSGI-KDLEVANM-TOM                   
002322     ELSE                                                                 
002323       MOVE MSGI-KDLEVANM-TOM      TO INIT-KDLEVANM-TOM                   
002324     END-IF                                                               
002325                                                                          
002326     IF MSGI-KDLEVANM              = ALL '+'                              
002327       MOVE INIT-KDLEVANM          TO MSGI-KDLEVANM                       
002328     ELSE                                                                 
002329       MOVE MSGI-KDLEVANM          TO INIT-KDLEVANM                       
002330     END-IF                                                               
002331                                                                          
002332     IF MSGI-KDLEVPLF              = ALL '+'                              
002333       MOVE INIT-KDLEVPLF          TO MSGI-KDLEVPLF                       
002334     ELSE                                                                 
002335       MOVE MSGI-KDLEVPLF          TO INIT-KDLEVPLF                       
002336     END-IF                                                               
002337                                                                          
002338     IF MSGI-KDLPORS               = ALL '+'                              
002339       MOVE INIT-KDLPORS           TO MSGI-KDLPORS                        
002340     ELSE                                                                 
002341       MOVE MSGI-KDLPORS           TO INIT-KDLPORS                        
002342     END-IF                                                               
002343                                                                          
002344     IF MSGI-KDMAIL                = ALL '+'                              
002345       MOVE INIT-KDMAIL            TO MSGI-KDMAIL                         
002346     ELSE                                                                 
002347       MOVE MSGI-KDMAIL            TO INIT-KDMAIL                         
002348     END-IF                                                               
002349                                                                          
002350     IF MSGI-KDORDKL               = ALL '+'                              
002351       MOVE INIT-KDORDKL           TO MSGI-KDORDKL                        
002352     ELSE                                                                 
002353       MOVE MSGI-KDORDKL           TO INIT-KDORDKL                        
002354     END-IF                                                               
002355                                                                          
002356     IF MSGI-KDOTFREK              = ALL '+'                              
002357       MOVE INIT-KDOTFREK          TO MSGI-KDOTFREK                       
002358     ELSE                                                                 
002359       MOVE MSGI-KDOTFREK          TO INIT-KDOTFREK                       
002360     END-IF                                                               
002361                                                                          
002362     IF MSGI-KDPRIO-PF             = ALL '+'                              
002363       MOVE INIT-KDPRIO-PF         TO MSGI-KDPRIO-PF                      
002364     ELSE                                                                 
002365       MOVE MSGI-KDPRIO-PF         TO INIT-KDPRIO-PF                      
002366     END-IF                                                               
002367                                                                          
002368     IF MSGI-KDPRODSL              = ALL '+'                              
002369       MOVE INIT-KDPRODSL          TO MSGI-KDPRODSL                       
002370     ELSE                                                                 
002371       MOVE MSGI-KDPRODSL          TO INIT-KDPRODSL                       
002372     END-IF                                                               
002373                                                                          
002374     IF MSGI-KDPRT                 = ALL '+'                              
002375       MOVE INIT-KDPRT             TO MSGI-KDPRT                          
002376     ELSE                                                                 
002377       MOVE MSGI-KDPRT             TO INIT-KDPRT                          
002378     END-IF                                                               
002379                                                                          
002380     IF MSGI-KDPRTVAL-ADR          = ALL '+'                              
002381       MOVE INIT-KDPRTVAL-ADR      TO MSGI-KDPRTVAL-ADR                   
002382     ELSE                                                                 
002383       MOVE MSGI-KDPRTVAL-ADR      TO INIT-KDPRTVAL-ADR                   
002384     END-IF                                                               
002385                                                                          
002386     IF MSGI-KDPRTVAL-FS           = ALL '+'                              
002387       MOVE INIT-KDPRTVAL-FS       TO MSGI-KDPRTVAL-FS                    
002388     ELSE                                                                 
002389       MOVE MSGI-KDPRTVAL-FS       TO INIT-KDPRTVAL-FS                    
002390     END-IF                                                               
002391                                                                          
002392     IF MSGI-KDSTARAD              = ALL '+'                              
002393       MOVE INIT-KDSTARAD          TO MSGI-KDSTARAD                       
002394     ELSE                                                                 
002395       MOVE MSGI-KDSTARAD          TO INIT-KDSTARAD                       
002396     END-IF                                                               
002397                                                                          
002398     IF MSGI-KDSTAPF               = ALL '+'                              
002399       MOVE INIT-KDSTAPF           TO MSGI-KDSTAPF                        
002400     ELSE                                                                 
002401       MOVE MSGI-KDSTAPF           TO INIT-KDSTAPF                        
002402     END-IF                                                               
002403                                                                          
002404     IF MSGI-KDRETSTA              = ALL '+'                              
002405       MOVE INIT-KDRETSTA          TO MSGI-KDRETSTA                       
002406     ELSE                                                                 
002407       MOVE MSGI-KDRETSTA          TO INIT-KDRETSTA                       
002408     END-IF                                                               
002409                                                                          
002410     IF MSGI-KDSORT                = ALL '+'                              
002411       MOVE INIT-KDSORT            TO MSGI-KDSORT                         
002412     ELSE                                                                 
002413       MOVE MSGI-KDSORT            TO INIT-KDSORT                         
002414     END-IF                                                               
002415                                                                          
002416     IF MSGI-KDSTOR                = ALL '+'                              
002417       MOVE INIT-KDSTOR            TO MSGI-KDSTOR                         
002418     ELSE                                                                 
002419       MOVE MSGI-KDSTOR            TO INIT-KDSTOR                         
002420     END-IF                                                               
002421                                                                          
002422     IF MSGI-KDTRPDOCT             = ALL '+'                              
002423       MOVE INIT-KDTRPDOCT         TO MSGI-KDTRPDOCT                      
002424     ELSE                                                                 
002425       MOVE MSGI-KDTRPDOCT         TO INIT-KDTRPDOCT                      
002426     END-IF                                                               
002427                                                                          
002428     IF MSGI-KDURVAL               = ALL '+'                              
002429       MOVE INIT-KDURVAL           TO MSGI-KDURVAL                        
002430     ELSE                                                                 
002431       MOVE MSGI-KDURVAL           TO INIT-KDURVAL                        
002432     END-IF                                                               
002433                                                                          
002434     IF MSGI-KDVALISO              = ALL '+'                              
002435       MOVE INIT-KDVALISO          TO MSGI-KDVALISO                       
002436     ELSE                                                                 
002437       MOVE MSGI-KDVALISO          TO INIT-KDVALISO                       
002438     END-IF                                                               
002439                                                                          
002440     IF MSGI-KDVALTYP              = ALL '+'                              
002441       MOVE INIT-KDVALTYP          TO MSGI-KDVALTYP                       
002442     ELSE                                                                 
002443       MOVE MSGI-KDVALTYP          TO INIT-KDVALTYP                       
002444     END-IF                                                               
002445                                                                          
002446     IF MSGI-KVBEART               = ALL '+'                              
002447       MOVE INIT-KVBEART           TO MSGI-KVBEART                        
002448     ELSE                                                                 
002449       MOVE MSGI-KVBEART           TO INIT-KVBEART                        
002450     END-IF                                                               
002451                                                                          
002452     IF MSGI-TEVORMRK              = ALL '+'                              
002453       MOVE INIT-TEVORMRK          TO MSGI-TEVORMRK                       
002454     ELSE                                                                 
002455       MOVE MSGI-TEVORMRK          TO INIT-TEVORMRK                       
002456     END-IF                                                               
002457                                                                          
002458     IF MSGI-TIAAVV                = ALL '+'                              
002459       MOVE INIT-TIAAVV            TO MSGI-TIAAVV                         
002460     ELSE                                                                 
002461       MOVE MSGI-TIAAVV            TO INIT-TIAAVV                         
002462     END-IF                                                               
002463                                                                          
002464     IF MSGI-TIAAVV-FOM            = ALL '+'                              
002465       MOVE INIT-TIAAVV-FOM        TO MSGI-TIAAVV-FOM                     
002466     ELSE                                                                 
002467       MOVE MSGI-TIAAVV-FOM        TO INIT-TIAAVV-FOM                     
002468     END-IF                                                               
002469                                                                          
002470     IF MSGI-TIAAVV-TOM            = ALL '+'                              
002471       MOVE INIT-TIAAVV-TOM        TO MSGI-TIAAVV-TOM                     
002472     ELSE                                                                 
002473       MOVE MSGI-TIAAVV-TOM        TO INIT-TIAAVV-TOM                     
002474     END-IF                                                               
002475                                                                          
002476     IF MSGI-TIAVIDAT              = ALL '+'                              
002477       MOVE INIT-TIAVIDAT          TO MSGI-TIAVIDAT                       
002478     ELSE                                                                 
002479       MOVE MSGI-TIAVIDAT          TO INIT-TIAVIDAT                       
002480     END-IF                                                               
002481                                                                          
002482     IF MSGI-TIFINLV-FOM           = ALL '+'                              
002483       MOVE INIT-TIFINLV-FOM       TO MSGI-TIFINLV-FOM                    
002484     ELSE                                                                 
002485       MOVE MSGI-TIFINLV-FOM       TO INIT-TIFINLV-FOM                    
002486     END-IF                                                               
002487                                                                          
002488     IF MSGI-TIFINLV-TOM           = ALL '+'                              
002489       MOVE INIT-TIFINLV-TOM       TO MSGI-TIFINLV-TOM                    
002490     ELSE                                                                 
002491       MOVE MSGI-TIFINLV-TOM       TO INIT-TIFINLV-TOM                    
002492     END-IF                                                               
002493                                                                          
002494     IF MSGI-TIKLOCK               = ALL '+'                              
002495       MOVE INIT-TIKLOCK           TO MSGI-TIKLOCK                        
002496     ELSE                                                                 
002497       MOVE MSGI-TIKLOCK           TO INIT-TIKLOCK                        
002498     END-IF                                                               
002499                                                                          
002500     IF MSGI-TIREGDAT              = ALL '+'                              
002501       MOVE INIT-TIREGDAT          TO MSGI-TIREGDAT                       
002502     ELSE                                                                 
002503       MOVE MSGI-TIREGDAT          TO INIT-TIREGDAT                       
002504     END-IF                                                               
002505                                                                          
002506     IF MSGI-TIREGTID              = ALL '+'                              
002507       MOVE INIT-TIREGTID          TO MSGI-TIREGTID                       
002508     ELSE                                                                 
002509       MOVE MSGI-TIREGTID          TO INIT-TIREGTID                       
002510     END-IF                                                               
002511                                                                          
002512     IF MSGI-TIRFSDAT-CDC          = ALL '+'                              
002513       MOVE INIT-TIRFSDAT-CDC      TO MSGI-TIRFSDAT-CDC                   
002514     ELSE                                                                 
002515       MOVE MSGI-TIRFSDAT-CDC      TO INIT-TIRFSDAT-CDC                   
002516     END-IF                                                               
002517                                                                          
002518     IF MSGI-TISKEPPN              = ALL '+'                              
002519       MOVE INIT-TISKEPPN          TO MSGI-TISKEPPN                       
002520     ELSE                                                                 
002521       MOVE MSGI-TISKEPPN          TO INIT-TISKEPPN                       
002522     END-IF                                                               
002523                                                                          
002524     IF MSGI-TISKROT-BEORD         = ALL '+'                              
002525       MOVE INIT-TISKROT-BEORD     TO MSGI-TISKROT-BEORD                  
002526     ELSE                                                                 
002527       MOVE MSGI-TISKROT-BEORD     TO INIT-TISKROT-BEORD                  
002528     END-IF                                                               
002529     .                                                                    
002530     EJECT                                                                
002531 F-KOLLA-TIDZON SECTION.                                                  
002532     MOVE WS-TILOKDAT TO TZ-TILOKDAT                                      
002533     MOVE WS-TILOKTID TO TZ-TILOKTID                                      
002534     MOVE INIT-IDTIDZON TO WS-IDTIDZON                                    
002535     MOVE SPACE TO MSGI-KDSVAR                                            
002536                                                                          
002537     MOVE TZ-TILOKDAT TO TZ-SKOTT-AR                                      
002538     IF TZ-TILOKDAT NOT NUMERIC                                           
002539         OR TZ-MM > 12 OR = 0                                             
002540         OR TZ-DD > 31  OR = 0                                            
002541         OR (TZ-MM = 4 OR 6 OR 9 OR 11) AND TZ-DD > 30                    
002542         OR (TZ-MM = 2 AND SKOTT-AR AND TZ-DD > 29)                       
002543         OR (TZ-MM = 2 AND NOT SKOTT-AR AND TZ-DD > 28)                   
002544       MOVE ' INVALID DATE' TO FELTEXT                                    
002545       MOVE 'F' TO MSGI-KDSVAR                                            
002546*      CALL FELLOG                                                        
002547     END-IF                                                               
002548                                                                          
002549     IF TZ-TILOKTID NOT NUMERIC                                           
002550         OR TZ-TIME-HOUR > 23                                             
002551       MOVE ' INVALID TIME'  TO FELTEXT                                   
002552       MOVE 'F' TO MSGI-KDSVAR                                            
002553*      CALL FELLOG                                                        
002554     END-IF                                                               
002555                                                                          
002556*    -- CHANGE FOR DAYLIGHT SAVING TIME IN COMPUTER                       
002557*    -- NOTE: COMPUTER TIME CHANGES ON SUNDAY NIGHT, NOT                  
002558*    -- MORNING. THEREFOR THE TEST IS > AND <=                            
002559     MOVE ZERO TO WS-DIFF                                                 
002560     MOVE +1 TO INDX                                                      
002561     PERFORM UNTIL TZRUL-IDTIDZON (INDX) NOT = SPACE                      
002562       IF TZ-TILOKDAT > TZRUL-DAT-FOM (INDX)                              
002563           AND TZ-TILOKDAT <= TZRUL-DAT-TOM (INDX)                        
002564         ADD TZRUL-DIFF (INDX) TO WS-DIFF                                 
002565       END-IF                                                             
002566       ADD +1 TO INDX                                                     
002567     END-PERFORM                                                          
002568                                                                          
002569*    -- CHANGE FOR DAYLIGHT SAVING TIME IN WAREHOUSE                      
002570*    -- NOTE: SUMMER/WINTER TIME CHANGES IN THE MORNING,                  
002571*    -- THEREFOR THE TEST IS >= AND <                                     
002572*    -- ALSO NOTE THAT SUMMER/WINTER TIME IS ASSUMED TO                   
002573*    -- START AT MIDNIGHT, NOT 02:00 OR 03:00                             
002574*    PERFORM UNTIL TZRUL-IDTIDZON (INDX) = '99'                           
002575*      IF INIT-IDTIDZON = TZRUL-IDTIDZON (INDX)                           
002576*        IF TZ-TILOKDAT >= TZRUL-DAT-FOM (INDX)                           
002577*            AND TZ-TILOKDAT < TZRUL-DAT-TOM (INDX)                       
002578*          ADD TZRUL-DIFF (INDX) TO WS-DIFF                               
002579*        END-IF                                                           
002580*      END-IF                                                             
002581*      ADD +1 TO INDX                                                     
002582*    END-PERFORM                                                          
           MOVE NEJ TO TZRUL-HIT-SW                                             
           PERFORM UNTIL TZRUL-IDTIDZON (INDX) = '99' OR                        
                              TZRUL-HIT-YES                                     
            IF INIT-IDTIDZON = TZRUL-IDTIDZON (INDX)                            
             IF (TZRUL-IDDC (INDX) = SPACE OR                                   
                       TZRUL-IDDC (INDX) = INIT-IDDC)                           
              IF TZ-TILOKDAT >= TZRUL-DAT-FOM (INDX)                            
                         AND TZ-TILOKDAT < TZRUL-DAT-TOM (INDX)                 
               ADD TZRUL-DIFF (INDX) TO WS-DIFF                                 
               MOVE JA TO TZRUL-HIT-SW                                          
              END-IF                                                            
             END-IF                                                             
            END-IF                                                              
            ADD +1 TO INDX                                                      
           END-PERFORM                                                          
002583     ADD WS-DIFF TO WS-IDTIDZON                                           
002584                                                                          
002585*    -- CONVERT THE YEAR OF THE INPUT DATE TO A FULL                      
002586*    -- 4-DIGIT YEAR, AND THEN CONVERT THE DATE TO AN                     
002587*    -- INTEGER VALUE (COUNTING FROM 1600)                                
002588     IF MSGI-KDSVAR = SPACE                                               
002589       IF TZ-AA > 50                                                      
002590         MOVE 19 TO TZ-TILOKDAT-CENT                                      
002591       ELSE                                                               
002592         MOVE 20 TO TZ-TILOKDAT-CENT                                      
002593       END-IF                                                             
002594     COMPUTE TZ-INT-DATE = FUNCTION INTEGER-OF-DATE (TZ-FULL-DATE)        
002595                                                                          
002596*    -- EXTRACT THE HOUR-PART OF THE INPUT TIME                           
002597       IF MSGI-KDCALL = '012'                                             
002598         COMPUTE TZ-DIFF = WS-IDTIDZON - 11                               
002599       ELSE                                                               
002600         COMPUTE TZ-DIFF = 11 - WS-IDTIDZON                               
002601       END-IF                                                             
002602       MOVE TZ-TIME-HOUR     TO TZ-HOUR                                   
002603       MOVE TZ-TIME-MIN      TO TZ-MIN                                    
002604                                                                          
002605*    --TO ADJUST TIME FOR INDIA                                           
002606       IF INIT-IDTIDZON = 07                                              
002607         ADD 30 TO TZ-MIN                                                 
002608         IF TZ-MIN > 59                                                   
002609           ADD 1 TO TZ-HOUR                                               
002610           SUBTRACT 60 FROM TZ-MIN                                        
002611         END-IF                                                           
002612       END-IF                                                             
002613                                                                          
002614*    -- AND COMPUTE THE NEW TIME AND POSSIBLY ALSO THE NEW DATE           
002615*        CONVERTS DATE AND TIME BETWEEN TWO TIME ZONES                    
002616                                                                          
002617       ADD TZ-DIFF TO TZ-HOUR                                             
002618       IF TZ-HOUR  < 0                                                    
002619         ADD 24 TO TZ-HOUR                                                
002620         SUBTRACT 1 FROM TZ-INT-DATE                                      
002621       END-IF                                                             
002622       IF TZ-HOUR  > 23                                                   
002623         SUBTRACT 24 FROM TZ-HOUR                                         
002624         ADD 1 TO TZ-INT-DATE                                             
002625       END-IF                                                             
002626                                                                          
002627*    -- CONVERT BACK TO DISPLAY FORMAT AND RETURN THE                     
002628*    -- VALUES IN THE OUTPUT FIELDS                                       
002629     COMPUTE TZ-FULL-DATE = FUNCTION DATE-OF-INTEGER (TZ-INT-DATE)        
002630       MOVE TZ-HOUR     TO TZ-TIME-HOUR                                   
002631       MOVE TZ-MIN      TO TZ-TIME-MIN                                    
002632                                                                          
002633       MOVE TZ-TILOKDAT TO MSGI-TILOKDAT                                  
002634       MOVE TZ-TILOKTID TO MSGI-TILOKTID                                  
002635     END-IF                                                               
002636     .                                                                    
002637     EJECT                                                                
002638 S-SECURITY     SECTION.                                                  
002639                                                                          
002640                                                                          
002641     MOVE INIT-KDARBTYP-SEC       TO MSGI-KDARBTYP-SEC                    
002642     MOVE INIT-KDARBTYP-SEC-IDLEV TO MSGI-KDARBTYP-SEC-IDLEV              
002643     MOVE INIT-KDARBTYP-SEC-4352  TO MSGI-KDARBTYP-SEC-4352               
002644     MOVE INIT-RESERV-SEC         TO MSGI-RESERV-SEC                      
002645     .                                                                    
002646     EJECT                                                                
002647 Z-FINIT        SECTION.                                                  
002648                                                                          
002649     IF WS-IDUSER-IDDC = 'WIDDC'                                          
002650          OR MSGI-IDTRANS = '0511'                                        
002651          OR MSGI-IDTRANS = '0513'                                        
002652          OR (MSGI-KDCALL = '011' OR '012' OR '013')                      
002653       CONTINUE                                                           
002654     ELSE                                                                 
002655       IF WS-IDUSER NOT = W-IDUSER                                        
002656         MOVE WS-IDUSER-LTERM TO INIT-BEANST                              
002657       END-IF                                                             
002658       IF SEGMENT-SAKNAS                                                  
002659         IF USEA-DBD-NAME = 'WDP7'                                        
002660           PERFORM IMS-ISRT-WDP7-ROT                                      
002661         ELSE                                                             
002662           PERFORM IMS-ISRT-USEA-ROT                                      
002663         END-IF                                                           
002664       ELSE                                                               
002665         PERFORM IMS-REPL-USEA                                            
002666       END-IF                                                             
002667     END-IF                                                               
002668     .                                                                    
002669     EJECT                                                                
002670* IMS SEKTIONER                                                           
002671                                                                          
002672 IMS-GET-USEA-ROT SECTION.                                                
002673     STRING 'WLUSEA01(IDUSER   =' W-WDP701KY-X ')'                        
002674            DELIMITED BY SIZE INTO SSA1                                   
002675     MOVE '  GE' TO GODK-STATUSKODER                                      
002676     CALL CBLTDLI USING GHU USEA-PCB DLI-IO-AREA SSA1                     
002677     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
002678     PERFORM IMS-STATUSKONTROLL                                           
002679     .                                                                    
002680     SKIP3                                                                
002681 IMS-GET-WDP7-ROT SECTION.                                                
002682     STRING 'WDP701  (IDUSER   =' W-WDP701KY-X ')'                        
002683            DELIMITED BY SIZE INTO SSA1                                   
002684     MOVE '  GE' TO GODK-STATUSKODER                                      
002685     CALL CBLTDLI USING GHU USEA-PCB DLI-IO-AREA SSA1                     
002686     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
002687     PERFORM IMS-STATUSKONTROLL                                           
002688     .                                                                    
002689     SKIP3                                                                
002690 IMS-ISRT-USEA-ROT SECTION.                                               
002691     MOVE 'WLUSEA01 ' TO SSA1                                             
002692     MOVE '  II' TO GODK-STATUSKODER                                      
002693     CALL CBLTDLI USING ISRT USEA-PCB DLI-IO-AREA SSA1                    
002694     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
002695     PERFORM IMS-STATUSKONTROLL                                           
002696     .                                                                    
002697     SKIP3                                                                
002698 IMS-ISRT-WDP7-ROT SECTION.                                               
002699     MOVE 'WDP701 ' TO SSA1                                               
002700     MOVE '  II' TO GODK-STATUSKODER                                      
002701     CALL CBLTDLI USING ISRT USEA-PCB DLI-IO-AREA SSA1                    
002702     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
002703     PERFORM IMS-STATUSKONTROLL                                           
002704     .                                                                    
002705     SKIP3                                                                
002706 IMS-REPL-USEA SECTION.                                                   
002707     MOVE '  ' TO GODK-STATUSKODER                                        
002708     CALL CBLTDLI USING REPL USEA-PCB DLI-IO-AREA                         
002709     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
002710     PERFORM IMS-STATUSKONTROLL                                           
002711     .                                                                    
002712     EJECT                                                                
002713 IMS-STATUSKONTROLL SECTION.                                              
002714     SET STATUS-IX TO 1                                                   
002715     SEARCH GODK-STATUS                                                   
002716       AT END                                                             
002717         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
002718         DELIMITED BY SIZE INTO FELTEXT                                   
002719         CALL FELLOG                                                      
002720       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
002721         CONTINUE                                                         
002730     END-SEARCH                                                           
002800     .                                                                    
