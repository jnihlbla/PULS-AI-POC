001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WL017700.                                                
001400 AUTHOR.         KJELLSON GÖRAN.                                          
001500 DATE-WRITTEN.   04/10/20.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       CARPARTS.LDC.UPDSELFILE                                  
001810*    WEB-LDC: WL017700 PROGRAM IS A REPLICA OF W5030500 PROGRAM           
001820*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        *                                                                
002110*        INVENTERING - INRAPPORTERING AV RE0-UPPGIFTER.                   
002200*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: WL0177T                                             
002700*        REQUEST:     WL0177I1                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        RESPONSE:    WL0177O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'WL017700'.            
004800                                                                          
004900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005110 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
005120 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
005130 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
005200                                                                          
005300 77  YES                         PIC X       VALUE 'J'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006300     88  KEYS-OK                             VALUE 'J'.                   
006400     88  KEYS-WRONG                          VALUE 'N'.                   
006410                                                                          
006420 01  FILLER                      PIC X(7)    VALUE 'DIVERSE'.             
006430 01  DIVERSE.                                                             
006431   03  FELFLAGGA                 PIC X       VALUE 'N'.                   
006440   03  RAPP-KOLL                 PIC X.                                   
006450       88  RAPPORT-FINNS                     VALUE 'J'.                   
006460       88  RAPPORT-SAKNAS                    VALUE 'N'.                   
006461   03  WX-IDDC.                                                           
006462       05  WS-IDDC-1             PIC X(2)    VALUE 'DC'.                  
006463       05  WS-IDDC-2             PIC X(2).                                
006464                                                                          
006470 01  WS-PARAMETRAR.                                                       
006480   03  WS-KVINVBEG-1             PIC  X(1).                               
006490   03  WS-KVINVBEG-2             PIC  X(2).                               
006491   03  WS-ADLAGOMR-1             PIC  X(1).                               
006492   03  WS-ADGANG-1               PIC  X(1).                               
006493   03  WS-ADPLATS-1              PIC  X(1).                               
006494   03  WS-ADPLATS-2              PIC  X(2).                               
006495   03  WS-ADPLATS-3              PIC  X(3).                               
006496   03  WS-ADPLATS-4              PIC  X(4).                               
006497   03  WS-KDPRODSL-1             PIC  X(1).                               
006498   03  WS-IDFKNGRP-1             PIC  X(1).                               
006499   03  WS-IDFKNGRP-2             PIC  X(2).                               
006500   03  WS-IDFKNGRP-3             PIC  X(3).                               
006501                                                                          
006502 01  BMP-PARAMETRAR.                                                      
006503   03  SKICKA-KVINVBEG           PIC  9(3).                               
006504   03  SKICKA-KDVVKL             PIC  9(1).                               
006505   03  SKICKA-ADLAGOMR           PIC  9(2).                               
006506   03  SKICKA-ADGANG             PIC  9(2).                               
006507   03  SKICKA-ADPLATS            PIC  9(5).                               
006508   03  SKICKA-KDPRODSL           PIC  9(2).                               
006509   03  SKICKA-IDFKNGRP           PIC  9(4).                               
006510     EJECT                                                                
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006710     03  WKPSKONV                PIC X(8)  VALUE 'WKPSKONV'.              
006720     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
006800     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
006900     03  ABEND                   PIC X(8)  VALUE 'ABEND   '.              
007000     03  WZ01SUB                 PIC X(8)  VALUE 'WZ01SUB '.              
007200     SKIP3                                                                
007210*                            WKPSKONV  PARAMETRAR                         
007220*01    -COPY WKPSAREA                                                     
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007900     SKIP3                                                                
008000 01  MESSAGE-CODES.                                                       
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008700     EJECT                                                                
008710*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009000     SKIP3                                                                
009100*01  -COPY WZ01SUB                                                        
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009400     SKIP3                                                                
009500 01  REQU-AREA.                                                           
009600*    03  -COPY WZ01REQU                                                   
009700*    03  -COPY WL0177I1                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010000     SKIP3                                                                
010100 01  RESP-AREA.                                                           
010200*    03  -COPY WZ01RESP                                                   
010300*    03  -COPY WL0177O1                                                   
010400     EJECT                                                                
010500*                                                                         
010510*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010520*                                                                         
010530 01    IMS-WS.                                                            
010540   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
010550     SKIP3                                                                
010560*                        **** STATUS-KOD FRÅN IMS                         
010570   03    STATUS-WS               PIC XX.                                  
010580     88    SEGMENT-FINNS                     VALUE '  '.                  
010590     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
010591     SKIP3                                                                
010592   03    GODK-STATUSKODER.                                                
010593     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
010594     SKIP3                                                                
010595 01    SSA1                      PIC X(64).                               
010596 01    SSA2                      PIC X(64).                               
010597     EJECT                                                                
010598*                            IMS FUNKTIONSKODER                           
010599*01    -COPY W0003                                                        
010600                                                                          
010601*01    -COPY WMFSAREA                                                     
010602                                                                          
010610 01  W-PROG-TO-PROG-SW.                                                   
010620*  03    -COPY WMSGSOP                                                    
010700                                                                          
011200 LINKAGE SECTION.                                                         
011210*01    -COPY W0009     -PRE MSG-                                          
011211                                                                          
011220*01    -COPY W0009     -PRE ALT-                                          
011230     EJECT                                                                
011300                                                                          
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB.                               
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
011700                                                                          
011900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012000     IF SUB-KDRC = 0                                                      
012100        PERFORM A-INIT                                                    
012200        PERFORM B-CHECK-KEYS                                              
012300        IF KEYS-OK                                                        
012400           PERFORM C-KOLLA-BEFRAPP                                        
012500           IF RAPPORT-FINNS                                               
012510              PERFORM D-KONTROLLERA-INDATA                                
012520              IF FELFLAGGA = NOO                                          
012521                 PERFORM E-STARTA-BMP                                     
012522                 MOVE '015' TO RESP-IDMSG-INFO                            
012530              END-IF                                                      
012600           END-IF                                                         
012800        END-IF                                                            
012900        PERFORM S02-RETURN-RESPONSE                                       
013000     END-IF                                                               
013200                                                                          
013500     MOVE ZERO TO RETURN-CODE                                             
013600     GOBACK                                                               
013700     .                                                                    
013800                                                                          
013900 A-INIT SECTION.                                                          
013910     MOVE 'A-INIT'      TO CURR-SECTION                                   
013920                                                                          
013930     MOVE ALL '+'   TO RESP-AREA                                          
013940     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
013950                       RESP-IDMSG-INFO                                    
013960                       RESP-IDELMT-ERROR                                  
013970     MOVE 001       TO RESP-IDMSGVER                                      
014700     .                                                                    
014800     EJECT                                                                
014900 B-CHECK-KEYS SECTION.                                                    
015000     MOVE 'B-CHECK-KEYS'    TO CURR-SECTION                               
015100                                                                          
015200     MOVE YES TO KEYS-SW                                                  
015300                                                                          
015400     IF REQU-KDPGMACT = 'E'                                               
015500        CONTINUE                                                          
015600     ELSE                                                                 
015700       MOVE '023'          TO RESP-IDMSG-ERROR                            
015800*      WRONG ACTION KEY ***                                               
015810       MOVE 'KDPGMACT'     TO RESP-IDELMT-ERROR                           
015811       MOVE NOO            TO KEYS-SW                                     
015812     END-IF                                                               
015820                                                                          
015822     MOVE REQU-IDDC-KEY TO WS-IDDC-2                                      
015823                           RESP-IDDC-KEY                                  
015900     .                                                                    
016000     EJECT                                                                
016100 C-KOLLA-BEFRAPP   SECTION.                                               
016101     MOVE 'C-KOLLA-BEFRAPP' TO CURR-SECTION                               
016110*                                                                         
016120*  KONTROLLERA OM NÅGON RAPPORTERING HAR GJORTS                           
016130*                                                                         
016140     MOVE NOO  TO RAPP-KOLL                                               
016150                                                                          
016160     IF REQU-KVINVBEG        NOT = ALL '+' OR                             
016170        REQU-KDVVKL          NOT = ALL '+' OR                             
016180        REQU-ADLAGOMR        NOT = ALL '+' OR                             
016190        REQU-ADGANG          NOT = ALL '+' OR                             
016191        REQU-ADPLATS         NOT = ALL '+' OR                             
016192        REQU-KDPRODSL        NOT = ALL '+' OR                             
016193        REQU-IDFKNGRP        NOT = ALL '+'                                
016194       MOVE YES TO RAPP-KOLL                                              
016195     END-IF                                                               
016196     .                                                                    
016197 D-KONTROLLERA-INDATA SECTION.                                            
016198     MOVE 'D-KONTROLLERA-INDATA' TO CURR-SECTION                          
016199*                                                                         
016200*  INDATAKONTROLL, FORMELLA KONTROLLER                                    
016201*                                                                         
016202     MOVE NOO TO FELFLAGGA                                                
016203                                                                          
016204     IF (REQU-INV-RE0-GRP NOT = ALL '+')                                  
016205        PERFORM DA-KONTROLLERA-KVINBERG                                   
016206        IF FELFLAGGA = NOO                                                
016207           PERFORM DB-KONTROLLERA-KDVVKL                                  
016208        END-IF                                                            
016209        IF FELFLAGGA = NOO                                                
016210           PERFORM DC-KONTROLLERA-ADLAGOMR                                
016220        END-IF                                                            
016230        IF FELFLAGGA = NOO                                                
016240           PERFORM DD-KONTROLLERA-ADGANG                                  
016250        END-IF                                                            
016260        IF FELFLAGGA = NOO                                                
016270           PERFORM DE-KONTROLLERA-ADPLATS                                 
016280        END-IF                                                            
016290        IF FELFLAGGA = NOO                                                
016300           PERFORM DF-KONTROLLERA-KDPRODSL                                
016310        END-IF                                                            
016320        IF FELFLAGGA = NOO                                                
016330           PERFORM DG-KONTROLLERA-IDFKNGRP                                
016340        END-IF                                                            
016350     END-IF                                                               
016403     .                                                                    
016404                                                                          
016405 DA-KONTROLLERA-KVINBERG SECTION.                                         
016406     MOVE 'DA-KONTROLLERA-KVINBERG' TO CURR-SECTION                       
016407*                                                                         
016408     IF REQU-KVINVBEG  = ALL '+'                                          
016409        MOVE YES               TO FELFLAGGA                               
016410        MOVE '026'             TO RESP-IDMSG-ERROR                        
016411*       MUST BE ENTERED          ***                                      
016412        MOVE 'KVINVBEG'        TO RESP-IDELMT-ERROR                       
016413     ELSE                                                                 
016421        IF REQU-KVINVBEG (2:2) = SPACE                                    
016422           MOVE REQU-KVINVBEG       TO WS-KVINVBEG-1                      
016423           MOVE WS-KVINVBEG-1       TO REQU-KVINVBEG                      
016424        ELSE                                                              
016425           IF REQU-KVINVBEG (3:1) = SPACE                                 
016426              MOVE REQU-KVINVBEG    TO WS-KVINVBEG-2                      
016427              MOVE WS-KVINVBEG-2    TO REQU-KVINVBEG                      
016428           END-IF                                                         
016429        END-IF                                                            
016430        INSPECT REQU-KVINVBEG REPLACING LEADING SPACE BY ZERO             
016431        IF REQU-KVINVBEG NOT NUMERIC                                      
016432           MOVE YES           TO FELFLAGGA                                
016433           MOVE '024'         TO RESP-IDMSG-ERROR                         
016434*          NOT NUMERIC            ***                                     
016435           MOVE 'KVINVBEG'    TO RESP-IDELMT-ERROR                        
016436        ELSE                                                              
016437           IF REQU-KVINVBEG < 0                                           
016438              MOVE YES        TO FELFLAGGA                                
016439              MOVE '023'      TO RESP-IDMSG-ERROR                         
016440*             LESS THAN ZERO          ***                                 
016441              MOVE 'KVINVBEG' TO RESP-IDELMT-ERROR                        
016442           ELSE                                                           
016443              MOVE REQU-KVINVBEG   TO SKICKA-KVINVBEG                     
016444              MOVE REQU-KVINVBEG   TO RESP-KVINVBEG-IN                    
016451           END-IF                                                         
016452        END-IF                                                            
016453     END-IF                                                               
016454     .                                                                    
016455                                                                          
016456 DB-KONTROLLERA-KDVVKL SECTION.                                           
016457     MOVE 'DB-KONTROLLERA-KDVVKL' TO CURR-SECTION                         
016458*                                                                         
016459     IF REQU-KDVVKL NOT = ALL '+'                                         
016460        IF REQU-KDVVKL NOT NUMERIC                                        
016461           MOVE YES          TO FELFLAGGA                                 
016462           MOVE '024'        TO RESP-IDMSG-ERROR                          
016463*          NOT NUMERIC            ***                                     
016464           MOVE 'KDVVKL'     TO RESP-IDELMT-ERROR                         
016465        ELSE                                                              
016466           IF REQU-KDVVKL = 0 OR 1 OR 2 OR 3 OR 4 OR 5                    
016467              MOVE REQU-KDVVKL    TO SKICKA-KDVVKL                        
016468              MOVE REQU-KDVVKL    TO RESP-KDVVKL-IN                       
016475           ELSE                                                           
016476              MOVE YES       TO FELFLAGGA                                 
016477              MOVE '023'     TO RESP-IDMSG-ERROR                          
016478*             WRONG KDVVKL           ***                                  
016479              MOVE 'KDVVKL'  TO RESP-IDELMT-ERROR                         
016480           END-IF                                                         
016481        END-IF                                                            
016482     ELSE                                                                 
016483        MOVE '0'                  TO SKICKA-KDVVKL                        
016484     END-IF                                                               
016485     .                                                                    
016486                                                                          
016487 DC-KONTROLLERA-ADLAGOMR SECTION.                                         
016488     MOVE 'DC-KONTROLLERA-ADLAGOMR' TO CURR-SECTION                       
016489*                                                                         
016490     IF REQU-ADLAGOMR NOT = ALL '+'                                       
016491        IF REQU-ADLAGOMR (2:1) = SPACE                                    
016492           MOVE REQU-ADLAGOMR       TO WS-ADLAGOMR-1                      
016493           MOVE WS-ADLAGOMR-1       TO REQU-ADLAGOMR                      
016500        END-IF                                                            
016600        INSPECT REQU-ADLAGOMR REPLACING LEADING SPACE BY ZERO             
016700        IF REQU-ADLAGOMR NOT NUMERIC                                      
016800           MOVE YES            TO FELFLAGGA                               
016900           MOVE '024'          TO RESP-IDMSG-ERROR                        
017000*          NOT NUMERIC            ***                                     
017100           MOVE 'ADLAGOMR'     TO RESP-IDELMT-ERROR                       
017200        ELSE                                                              
017300           MOVE REQU-ADLAGOMR  TO SKICKA-ADLAGOMR                         
017320           MOVE REQU-ADLAGOMR  TO RESP-ADLAGOMR-IN                        
017400        END-IF                                                            
017500     ELSE                                                                 
017600        MOVE '00'                   TO SKICKA-ADLAGOMR                    
017700     END-IF                                                               
017800     .                                                                    
017900                                                                          
018000 DD-KONTROLLERA-ADGANG SECTION.                                           
018100     MOVE 'DD-KONTROLLERA-ADGANG' TO CURR-SECTION                         
018200*                                                                         
018300     IF REQU-ADGANG NOT = ALL '+'                                         
018310        IF REQU-ADGANG (2:1) = SPACE                                      
018320           MOVE REQU-ADGANG       TO WS-ADGANG-1                          
018330           MOVE WS-ADGANG-1       TO REQU-ADGANG                          
018340        END-IF                                                            
018350        INSPECT REQU-ADGANG REPLACING LEADING SPACE BY ZERO               
018360        IF REQU-ADGANG NOT = '00'                                         
018370           IF REQU-ADGANG NOT NUMERIC                                     
018380              MOVE YES       TO FELFLAGGA                                 
018390              MOVE '024'     TO RESP-IDMSG-ERROR                          
018391*             NOT NUMERIC            ***                                  
018392              MOVE 'ADGANG'  TO RESP-IDELMT-ERROR                         
018393           ELSE                                                           
018394              MOVE REQU-ADGANG    TO SKICKA-ADGANG                        
018395              MOVE REQU-ADGANG    TO RESP-ADGANG-IN                       
018399           END-IF                                                         
018400        ELSE                                                              
018401           MOVE '00'              TO SKICKA-ADGANG                        
018402        END-IF                                                            
018403     ELSE                                                                 
018404        MOVE '00'                 TO SKICKA-ADGANG                        
018405     END-IF                                                               
018406     .                                                                    
018407                                                                          
018408 DE-KONTROLLERA-ADPLATS SECTION.                                          
018409     MOVE 'DE-KONTROLLERA-ADPLATS'    TO CURR-SECTION                     
018410*                                                                         
018411     IF REQU-ADPLATS NOT = ALL '+'                                        
018412        IF REQU-ADPLATS (2:4) = SPACE                                     
018413           MOVE REQU-ADPLATS          TO WS-ADPLATS-1                     
018414           MOVE WS-ADPLATS-1          TO REQU-ADPLATS                     
018415        ELSE                                                              
018416           IF REQU-ADPLATS (3:3) = SPACE                                  
018417              MOVE REQU-ADPLATS       TO WS-ADPLATS-2                     
018418              MOVE WS-ADPLATS-2       TO REQU-ADPLATS                     
018419           ELSE                                                           
018420              IF REQU-ADPLATS (4:2) = SPACE                               
018421                 MOVE REQU-ADPLATS    TO WS-ADPLATS-3                     
018422                 MOVE WS-ADPLATS-3    TO REQU-ADPLATS                     
018423              ELSE                                                        
018424                 IF REQU-ADPLATS (5:1) = SPACE                            
018425                    MOVE REQU-ADPLATS TO WS-ADPLATS-4                     
018426                    MOVE WS-ADPLATS-4 TO REQU-ADPLATS                     
018427                 END-IF                                                   
018428              END-IF                                                      
018429           END-IF                                                         
018430        END-IF                                                            
018431        INSPECT REQU-ADPLATS REPLACING LEADING SPACE BY ZERO              
018432        IF REQU-ADPLATS NOT = '00000'                                     
018433           IF REQU-ADPLATS NOT NUMERIC                                    
018434              MOVE YES         TO FELFLAGGA                               
018435              MOVE '024'       TO RESP-IDMSG-ERROR                        
018436*             NOT NUMERIC            ***                                  
018437              MOVE 'ADPLATS'   TO RESP-IDELMT-ERROR                       
018438           ELSE                                                           
018439              MOVE REQU-ADPLATS   TO SKICKA-ADPLATS                       
018440              MOVE REQU-ADPLATS   TO RESP-ADPLATS-IN                      
018443           END-IF                                                         
018444        ELSE                                                              
018445           MOVE '00000'               TO SKICKA-ADPLATS                   
018446        END-IF                                                            
018447     ELSE                                                                 
018448        MOVE '00000'                  TO SKICKA-ADPLATS                   
018449     END-IF                                                               
018450     .                                                                    
018451                                                                          
018452 DF-KONTROLLERA-KDPRODSL SECTION.                                         
018453     MOVE 'DF-KONTROLLERA-KDPRODSL'   TO CURR-SECTION                     
018454*                                                                         
018455     IF REQU-KDPRODSL NOT = ALL '+'                                       
018456        IF REQU-KDPRODSL (2:1) = SPACE                                    
018457           MOVE REQU-KDPRODSL         TO WS-KDPRODSL-1                    
018458           MOVE WS-KDPRODSL-1         TO REQU-KDPRODSL                    
018459        END-IF                                                            
018460        INSPECT REQU-KDPRODSL REPLACING LEADING SPACE BY ZERO             
018461        IF REQU-KDPRODSL NOT NUMERIC                                      
018462           MOVE YES              TO FELFLAGGA                             
018463           MOVE '024'            TO RESP-IDMSG-ERROR                      
018464*          NOT NUMERIC            ***                                     
018465           MOVE 'KDPRODSL'       TO RESP-IDELMT-ERROR                     
018466        ELSE                                                              
018467           MOVE REQU-KDPRODSL         TO KPS-KDPRODSL                     
018468           MOVE 002                   TO KPS-KDCALL                       
018469           MOVE SPACE                 TO KPS-FLPRODSL                     
018470           CALL WKPSKONV USING KPS-WKPSAREA                               
018471***** TEST OM PRODUKTSLAG EXISTERAR OCH ÄR GILTIGT NU                     
018472           IF KPS-FLPRODSL = YES                                          
018473              MOVE REQU-KDPRODSL  TO SKICKA-KDPRODSL                      
018474              MOVE REQU-KDPRODSL  TO RESP-KDPRODSL-IN                     
018476           ELSE                                                           
018477              MOVE YES           TO FELFLAGGA                             
018478              MOVE '023'         TO RESP-IDMSG-ERROR                      
018479*             INVALID KDPRODSL       ***                                  
018480              MOVE 'KDPRODSL'    TO RESP-IDELMT-ERROR                     
018481           END-IF                                                         
018482        END-IF                                                            
018483     ELSE                                                                 
018484        MOVE '00' TO SKICKA-KDPRODSL                                      
018485     END-IF                                                               
018486     .                                                                    
018487                                                                          
018488 DG-KONTROLLERA-IDFKNGRP  SECTION.                                        
018489     MOVE 'DF-KONTROLLERA-IDFKNGRP' TO CURR-SECTION                       
018490*                                                                         
018491     IF REQU-IDFKNGRP NOT = ALL '+'                                       
018492        IF REQU-IDFKNGRP (2:3) = SPACE                                    
018493           MOVE REQU-IDFKNGRP       TO WS-IDFKNGRP-1                      
018494           MOVE WS-IDFKNGRP-1       TO REQU-IDFKNGRP                      
018495        ELSE                                                              
018496           IF REQU-IDFKNGRP (3:2) = SPACE                                 
018497              MOVE REQU-IDFKNGRP    TO WS-IDFKNGRP-2                      
018498              MOVE WS-IDFKNGRP-2    TO REQU-IDFKNGRP                      
018499           ELSE                                                           
018500              IF REQU-IDFKNGRP (4:1) = SPACE                              
018501                 MOVE REQU-IDFKNGRP TO WS-IDFKNGRP-3                      
018502                 MOVE WS-IDFKNGRP-3 TO REQU-IDFKNGRP                      
018503              END-IF                                                      
018504           END-IF                                                         
018505        END-IF                                                            
018506        INSPECT REQU-IDFKNGRP REPLACING LEADING SPACE BY ZERO             
018507        IF REQU-IDFKNGRP NOT NUMERIC                                      
018508           MOVE YES                 TO FELFLAGGA                          
018509           MOVE '024'               TO RESP-IDMSG-ERROR                   
018510*          NOT NUMERIC            ***                                     
018511           MOVE 'IDFKNGRP'          TO RESP-IDELMT-ERROR                  
018512        ELSE                                                              
018513           MOVE REQU-IDFKNGRP       TO SKICKA-IDFKNGRP                    
018514           MOVE REQU-IDFKNGRP       TO RESP-IDFKNGRP-IN                   
018515        END-IF                                                            
018516     ELSE                                                                 
018517        MOVE '0000'                 TO SKICKA-IDFKNGRP                    
018518     END-IF                                                               
018519     .                                                                    
018520                                                                          
018521 E-STARTA-BMP SECTION.                                                    
018522     MOVE 'E-STARTA-BMP'  TO CURR-SECTION                                 
018523*                                                                         
018524*  STARTA BMP W513B1                                                      
018525*                                                                         
018526     MOVE '5305'        TO MSGSOP-IDTRANS                                 
018527     MOVE MFS-KDMFSFOR  TO MSGSOP-KDMFSFOR                                
018528     MOVE 'W513B1'      TO MSGSOP-IDPROCESS                               
018529     MOVE 'O'           TO MSGSOP-KDSOPFUNK                               
018530                                                                          
018531     STRING 'IDDC(' WX-IDDC ') KVINVBEG(' SKICKA-KVINVBEG ') KDVVK        
018540-           'L(' SKICKA-KDVVKL ') ADLAGOMR(' SKICKA-ADLAGOMR ') AD        
018550-           'GANG(' SKICKA-ADGANG ') ADPLATS(' SKICKA-ADPLATS ') K        
018560-           'DPRODSL(' SKICKA-KDPRODSL ')                                 
018570-           'IDFKNGRP(' SKICKA-IDFKNGRP ')                                
018580-           'IDRT(' REQU-IDRT-KEY ')'                                     
018590          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
018591                                                                          
018600     PERFORM IMS-INSERT-ALTMSG                                            
018700     .                                                                    
019200*    --- DISPATCHER SECTIONS                                              
019300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019400                                                                          
019500     MOVE 'GETARG'                  TO SUB-KDFUNC                         
019600     MOVE 'CARPARTS.LDC.UPDSELFILE' TO SUB-ADDISPABS                      
019700     MOVE LENGTH OF REQU-AREA       TO SUB-KVDLEN                         
019800                                                                          
019900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
020000                                                                          
020100     IF SUB-KDRC > 0                                                      
020200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
020400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020600     END-IF                                                               
020700     .                                                                    
020800                                                                          
020900 S02-RETURN-RESPONSE SECTION.                                             
021000                                                                          
021100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
021300                                                                          
021400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021500                                                                          
021600     IF SUB-KDRC > 0                                                      
021700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022100     END-IF                                                               
022200     .                                                                    
022210                                                                          
022300 IMS-INSERT-ALTMSG SECTION.                                               
022400     MOVE '  ' TO GODK-STATUSKODER                                        
022500     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
022600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     .                                                                    
022900     EJECT                                                                
023000 IMS-STATUSKONTROLL SECTION.                                              
023100                                                                          
023200     SET STATUS-IX TO 1                                                   
023300     SEARCH GODK-STATUS AT END CALL FELLOG                                
023400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
023500     END-SEARCH                                                           
023600     .                                                                    
