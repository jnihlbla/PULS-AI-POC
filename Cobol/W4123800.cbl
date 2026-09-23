000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4123800.                                    
000300 AUTHOR.                     GERRY CARMICHAEL.                            
000400     DATE-WRITTEN.           JUNI 2000.                                   
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    LÄSER ORDERHUVUDETSREGISTRET (WDQ2) MED SB                           
001100*                            OCH   WDB2.                                  
001200*    SKRIVER FIL MED TACDIS ORDRAR.                                       
001210*                                                                         
001220*    STORY 2375089/ADD IDSYSTEM VOUI, ECOM                                
001230*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600*                                                                         
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*    ---- UT-FIL W41238    OUTPUT                                         
002000                                                                          
002100     SELECT W41238           ASSIGN TO      W41238D1.                     
002200     EJECT                                                                
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W41238                                                               
002900     LABEL RECORD STANDARD                                                
003000     RECORDING F                                                          
003100     BLOCK CONTAINS 0.                                                    
003200                                                                          
003300*01  POST -COPY W41238 -PRE  UT- -L.                                      
003400     EJECT                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP2                                                                
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000     SKIP3                                                                
004100*    ---- GENERELLA KONSTANTER                                            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004600 77  WC-CDC-11                   PIC X(2)    VALUE '11'.                  
004700                                                                          
004701*    DC FÖR CLEARING AV VERKSTADSORDER                                    
004710*01  -COPY WWDC01                                                         
004720                                                                          
004800*    ---- EOF-SWITCHAR                                                    
004900 77  W41238-EOF                  PIC X       VALUE 'N'.                   
005000                                                                          
005100*    ---- SWITCHAR                                                        
060400 77  KDORDSTA-PRIM-SW            PIC X       VALUE 'N'.                   
060500     88  KDORDSTA-PRIM                       VALUE 'J'.                   
060501                                                                          
060502*    ---- ARBETSFÄLT                                                      
060503 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
060504 01  FILLER REDEFINES DAGENS-DATUM.                                       
060505     03  DAGENS-DATUM-AAR        PIC 9(2).                                
060506     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
060507     03  DAGENS-DATUM-DAG        PIC 9(2).                                
060508     SKIP3                                                                
060509 01  WS-TIRFS                    PIC 9(10)   VALUE ZERO.                  
060510 01  FILLER REDEFINES WS-TIRFS.                                           
060511     03  WS-TIRFS-DATUM          PIC 9(6).                                
060512     03  WS-TIRFS-TID            PIC 9(4).                                
060513 01  WS-DARFS                    PIC 9(12)   VALUE ZERO.                  
060514 01  FILLER REDEFINES WS-DARFS.                                           
060515     03  WS-DARFS-SEKEL          PIC 9(2).                                
060516     03  WS-DARFS-DATUM-TID      PIC 9(10).                               
060517                                                                          
060518     EJECT                                                                
060519                                                                          
060520 01  FELTEXT.                                                             
060521     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
060522     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
060523     EJECT                                                                
060524                                                                          
060525 01  RETURKODER.                                                          
060526   03  RKOD                      PIC S9(4) COMP SYNC VALUE ZERO.          
060527   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4) COMP SYNC VALUE +16.           
060528   03  RKOD-ABEND-MED-DUMP       PIC S9(4) COMP SYNC VALUE +33.           
060529                                                                          
060530*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
060531                                                                          
060532 01  DYNAMISKA-SUBPROGRAM.                                                
060533   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
060534   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
060535   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
060536   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
060537   03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY '.            
060538     SKIP3                                                                
060539                                                                          
060540 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
060541*   -COPY WORKAREA                                                        
060542     EJECT                                                                
060543                                                                          
060544*    ---- PARAMETRAR TILL POSTSUM                                         
060545*01  -COPY W0005      -PRE POSTSUM-.                                      
060546     EJECT                                                                
060547                                                                          
060548*01  FILLER                      PIC X(8)    VALUE 'UT-AREA'.             
060549                                                                          
060550*01  AREA -COPY W41238     -PRE UT-                                       
060551     EJECT                                                                
060552                                                                          
060553*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
060554                                                                          
060555 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
060556                                                                          
060557*    ---- STATUSKOD FRÅN IMS                                              
060558                                                                          
060559 01  STATUS-WS                   PIC XX.                                  
060560     88  SEGMENT-FINNS                      VALUE '  '.                   
060561     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
060562     88  SEGMENT-SLUT                       VALUE 'GB'.                   
060563     SKIP3                                                                
060564 01  GODK-STATUSKODER.                                                    
060565   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
060566     SKIP3                                                                
060567 01  SSA1                        PIC X(64).                               
060568     SKIP3                                                                
060569                                                                          
060570*01      -COPY W0003.                                                     
060571                                                                          
060572     EJECT                                                                
060573 01  NYCKLAR-TILL-DLI.                                                    
060574     03  W-IDGMT-X.                                                       
060575         05  W-IDDISTR           PIC S9(5)  COMP-3.                       
060576         05  W-IDKUNDNR          PIC S9(7)  COMP-3.                       
060577                                                                          
060578 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDB201'.           
060579 01  DLI-IO-WDB201.                                                       
060580*    03  -COPY WDB201                                                     
060581     EJECT                                                                
060582 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA'.             
060583 01  DLI-IO-AREA.                                                         
060584   03  IO-AREA               PIC X(5000)  VALUE SPACE.                    
060585     SKIP2                                                                
060586*  03  WLORQI01     -COPY WDQ201             -RED IO-AREA.                
060587     EJECT                                                                
060588*  03  WLORQI12     -COPY WDQ212             -RED IO-AREA.                
060589                                                                          
060590                                                                          
060591 LINKAGE SECTION.                                                         
060592     SKIP2                                                                
060593*01  -COPY W0008  -PRE WDQ2-.                                             
060594    05  FILLER                   PIC X.                                   
060595     EJECT                                                                
060596*01  -COPY W0008  -PRE WDB2-                                              
060597     05  FILLER                  PIC X.                                   
060598     EJECT                                                                
060599                                                                          
060600 PROCEDURE DIVISION  USING WDQ2-PCB WDB2-PCB.                             
060601     ENTRY 'DLITCBL' USING WDQ2-PCB WDB2-PCB.                             
060602 STYR SECTION.                                                            
060603     PERFORM A-INIT                                                       
060604     PERFORM IMS-GET-WDQ2                                                 
060605     PERFORM UNTIL SEGMENT-SLUT                                           
060606       EVALUATE WDQ2-SEG-NAME-FB                                          
060607         WHEN 'WDQ201  '                                                  
060608           PERFORM B-KOLLA-ORDER                                          
060609         WHEN 'WDQ212  '                                                  
060610           PERFORM C-EV-SKAPA-UTPOST                                      
060611       END-EVALUATE                                                       
060612       PERFORM IMS-GET-WDQ2                                               
060613     END-PERFORM                                                          
060614                                                                          
060615     PERFORM Z-FINIT                                                      
060616     MOVE ZERO TO RETURN-CODE                                             
060617     GOBACK                                                               
060618     .                                                                    
060619     EJECT                                                                
060620 A-INIT SECTION.                                                          
060621     SKIP2                                                                
060622     OPEN OUTPUT W41238                                                   
060623     MOVE 'W4123800'         TO POSTSUM-PROGNAMN                          
060624     MOVE 'W41238D1'         TO POSTSUM-DDNAMN2                           
060625     MOVE 'W41238  '         TO POSTSUM-FDNAMN                            
060626                                                                          
060627     MOVE SPACE TO UT-AREA                                                
060628     ACCEPT DAGENS-DATUM FROM DATE                                        
060629     .                                                                    
060630                                                                          
060631     EJECT                                                                
060632 B-KOLLA-ORDER  SECTION.                                                  
060633                                                                          
060635     MOVE SPACE TO UT-AREA                                                
060636     MOVE 'N'   TO KDORDSTA-PRIM-SW                                       
060637                                                                          
060638     IF (OHUV-IDSYSTEM = 'LDC ' OR                                        
060639         OHUV-IDSYSTEM(1:3) = 'LYN' OR                                    
060640        (OHUV-IDSYSTEM(1:3) = 'ECO' AND                                   
060640         OHUV-TIREPDAT > 0)         OR                                    
060641         OHUV-IDSYSTEM(1:3) = 'VOU' OR                                    
060642         OHUV-IDSYSTEM(1:3) = 'TAD' OR                                    
060643         OHUV-IDSYSTEM(1:3) = 'ACC' OR                                    
060644         OHUV-IDSYSTEM(1:3) = 'APA' OR                                    
060645         OHUV-IDSYSTEM(1:3) = 'APB' OR                                    
060646         OHUV-IDSYSTEM(1:3) = 'APC' OR                                    
060647         OHUV-IDSYSTEM(1:3) = 'APD' OR                                    
060648         OHUV-IDSYSTEM(1:3) = 'APE' OR                                    
060649         OHUV-IDSYSTEM(1:3) = 'APF' OR                                    
060650         OHUV-IDSYSTEM(1:3) = 'APG' OR                                    
060651         OHUV-IDSYSTEM(1:3) = 'APH' OR                                    
060652         OHUV-IDSYSTEM(1:3) = 'API' OR                                    
060653         OHUV-IDSYSTEM(1:3) = 'APJ')                                      
060654        MOVE OHUV-IDDC-PRIM     TO DC01-IDDC                              
060655        IF REPAIR-CLEARING OR REPAIR-CLEARING-SDC                         
060657           MOVE OHUV-IDORDER            TO UT-IDORDER                     
060658           MOVE OHUV-IDDC-PRIM          TO UT-IDDC                        
060659           MOVE OHUV-IDDISTR            TO UT-IDDISTR                     
060660                                           W-IDDISTR                      
060661           MOVE OHUV-IDKUNDNR           TO UT-IDKUNDNR                    
060662                                           W-IDKUNDNR                     
060663           MOVE OHUV-IDORDNR7           TO UT-IDORDNR7                    
060664           MOVE OHUV-BEKUNDRF           TO UT-BEKUNDRF                    
060665           MOVE OHUV-FLEMBORD           TO UT-FLEMBORD                    
060666           MOVE OHUV-FLFORBI            TO UT-FLFORBI                     
060667           MOVE OHUV-FLORDSPE           TO UT-FLORDSPE                    
060668           MOVE OHUV-FLOVRLEV           TO UT-FLOVRLEV                    
060669           MOVE OHUV-KDFAKTYP           TO UT-KDFAKTYP                    
060670           MOVE OHUV-KDORDKL            TO UT-KDORDKL                     
060672        END-IF                                                            
060673     END-IF                                                               
060674     .                                                                    
060675     EJECT                                                                
060676 C-EV-SKAPA-UTPOST SECTION.                                               
060677                                                                          
060678     IF ARB-IDDC        = OHUV-IDDC-PRIM AND                              
060679        ARB-KDORDSTA (1:1) = 'R'                                          
060680        MOVE 'J'      TO KDORDSTA-PRIM-SW                                 
060681     END-IF                                                               
060682                                                                          
060683     IF UT-AREA NOT = SPACE AND                                           
060690        KDORDSTA-PRIM                                                     
060691                                                                          
060692        MOVE ARB-IDDC TO DC01-IDDC                                        
060693        IF REPAIR-CLEARING OR REPAIR-CLEARING-SDC                         
060694           PERFORM IMS-GU-WDB201                                          
060695                                                                          
060696           IF (REPAIR-CLEARING OR REPAIR-CLEARING-SDC) AND                
060697              GMT-FLLDCKND = JA                                           
060698              IF ARB-TIRFS > ZERO                                         
060699                 MOVE ARB-IDDC    TO UT-IDDC                              
060700                 PERFORM CA-BERAKNA-CLEARING-DATUM                        
060701                 MOVE ARB-TIRFS   TO WS-TIRFS                             
060702                 IF WS-TIRFS-DATUM <= WORK-TIAAMMDD-NEXT-WORKDAY          
060703                    MOVE 20       TO WS-DARFS-SEKEL                       
060704                    MOVE WS-TIRFS TO WS-DARFS-DATUM-TID                   
060705                    MOVE WS-DARFS TO UT-DARFS                             
060706                                                                          
060707                    PERFORM S11-SKRIV-W41238                              
060708                 END-IF                                                   
060709              END-IF                                                      
060710           END-IF                                                         
060716        END-IF                                                            
060717     END-IF                                                               
060718     .                                                                    
060719     EJECT                                                                
060720 CA-BERAKNA-CLEARING-DATUM SECTION.                                       
060721                                                                          
060722** RÄKNAR X ARBETSDAGAR FRAMÅT                                            
060723     MOVE WC-CDC-11             TO WORK-IDDC                              
060724     MOVE +002                  TO WORK-KDCALL                            
060725     MOVE GMT-KVDAGAR-CDC       TO WORK-KVWORKD                           
060726     MOVE DAGENS-DATUM          TO WORK-TIAAMMDD-FOM                      
060727     CALL WORKDAY               USING WORK-KDCALL                         
060728                                      WORK-DATE-AREA                      
060729                                      WORK-KDSVAR                         
060730     IF WORK-KDSVAR-FEL                                                   
060731        MOVE 'SECT CA-, DATUM SAKNAS I WORKDAY'                           
060732                                TO    FELTEXT-STR                         
060733        CALL ABEND              USING RKOD-ABEND-MED-DUMP                 
060734     END-IF                                                               
060735     .                                                                    
060736     EJECT                                                                
060737 S11-SKRIV-W41238 SECTION.                                                
060738                                                                          
060739     WRITE UT-POST FROM UT-AREA                                           
060740                                                                          
060741     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
060742     MOVE 'W41238' TO POSTSUM-FDNAMN                                      
060743     MOVE 'W41238D1' TO POSTSUM-DDNAMN2                                   
060744     CALL POSTSUM USING POSTSUM-PARM                                      
060745     .                                                                    
060746     EJECT                                                                
060747 Z-FINIT SECTION.                                                         
060748     SKIP2                                                                
060749     CLOSE W41238                                                         
060750     MOVE 'S' TO POSTSUM-OPKOD                                            
060751     CALL POSTSUM USING POSTSUM-PARM                                      
060752     .                                                                    
060753     EJECT                                                                
060754*    ---- IMS SEKTIONER                                                   
060755 IMS-GET-WDQ2 SECTION.                                                    
060756                                                                          
060757     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
060758     CALL CBLTDLI USING GN WDQ2-PCB DLI-IO-AREA                           
060759     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
060760     PERFORM IMS-STATUSKONTROLL                                           
060761     .                                                                    
060762     SKIP3                                                                
060763 IMS-GU-WDB201 SECTION.                                                   
060764                                                                          
060765     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
060766          DELIMITED BY SIZE INTO SSA1                                     
060767     MOVE '  ' TO GODK-STATUSKODER                                        
060768     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
060769     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
060770     PERFORM IMS-STATUSKONTROLL                                           
060771     .                                                                    
060772                                                                          
060773 IMS-STATUSKONTROLL SECTION.                                              
060774                                                                          
060775     SET STATUS-IX TO 1                                                   
060776     SEARCH GODK-STATUS                                                   
060777       AT END CALL FELLOG                                                 
060778       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
060779     END-SEARCH                                                           
060780     .                                                                    
060790     EJECT                                                                
