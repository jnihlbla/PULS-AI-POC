001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W4266900.                                                
001400*AUTHOR.         ANN WESTBERG.                                            
001500*DATE-WRITTEN.   92/11/16.                                                
001600                                                                          
001700*    REMARKS.                                                             
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        RENSAR W6L1 MED SAMMA LÖPNUMMER SOM LÄGGS UPP PÅ HISTFIL         
002100*        W42670.                                                          
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR W6UPFA (W6L1)                              
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003601     SKIP2                                                                
003602*          --- LÖPNR FÖR RENSNING AV W6L1                                 
003610     SELECT W42669                     ASSIGN TO W42669D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W42669                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205     SKIP2                                                                
004210*01  -COPY W4266901      -L.                                              
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W4266900'.            
004700 01  CHKP-VAR.                                                            
004800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-ANT                    PIC S9(3)   VALUE +0  COMP-3.            
005300 03  CHKP-MAX                    PIC S9(3)   VALUE +99 COMP-3.            
005310 03  POST-ANT                    PIC S9(3)   VALUE +0  COMP-3.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005510 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE ZERO COMP SYNC.        
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101                                                                          
006102 77  W42669-EOF-SW               PIC X       VALUE 'N'.                   
006110     88  END-OF-W42669                       VALUE 'J'.                   
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007510     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007520     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
007601     EJECT                                                                
007602*    --- PARAMETRAR TILL POSTSUM                                          
007603*                                                                         
007610*01  -COPY W0005   -PRE  POSTSUM-                                         
007901     EJECT                                                                
007902 01  IN-AREA-START               PIC X(24)   VALUE                        
007903                                             'IN-AREA-START'.             
007904     SKIP2                                                                
007905                                                                          
007910*01  AREA -COPY W4266901     -PRE IN-                                     
008000*                                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008501     03  W-IDLOPNRM-X.                                                    
008510         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
008511                                                                          
008520     03  W-IDHTYP-X.                                                      
008530         05  W-IDHTYP            PIC  X(4)   VALUE SPACE.                 
008540         05  NYCKEL-VALFRI       PIC  X(26).                              
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(64).                               
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010700     SKIP3                                                                
010800 01  DLI-IO-AREA-UPFA.                                                    
010900     03  W6UPFA01.                                                        
011000         05  -COPY W6L101                                                 
011001     EJECT                                                                
011200 01  FILLER                      PIC X(16)                                
011300                             VALUE 'DLI-IO-AREA-2'.                       
011500 01  DLI-IO-AREA-CKPD.                                                    
011600     03  W6CKPD11.                                                        
011700         05  -COPY W6GX6022                                               
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009   -PRE MSG-                                              
012401     EJECT                                                                
012402*01  -COPY W0008  -PRE UPFA-                                              
012410     05  FILLER                  PIC X.                                   
012420*01  -COPY W0008  -PRE CKPD-                                              
012430     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012801 PROCEDURE DIVISION  USING MSG-PCB UPFA-PCB CKPD-PCB.                     
012810     ENTRY 'DLITCBL' USING MSG-PCB UPFA-PCB CKPD-PCB.                     
012900                                                                          
013100     SKIP2                                                                
013200     PERFORM A-INIT                                                       
013300     PERFORM IMS-RESTART                                                  
013301     PERFORM IMS-LAS-ATERSTART                                            
013302                                                                          
013303     IF 6022-KVPOST > +0                                                  
013304       PERFORM B-LAS-FRAM-TILL-CHKPOINT                                   
013305     ELSE                                                                 
013306       PERFORM S01-LAES-W42669                                            
013307     END-IF                                                               
013308                                                                          
013400     PERFORM UNTIL END-OF-W42669                                          
013410       PERFORM C-BEHANDLA-INDATA                                          
013420       PERFORM S01-LAES-W42669                                            
013430     END-PERFORM                                                          
013440                                                                          
014800     PERFORM Z-FINIT                                                      
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500                                                                          
015910     OPEN INPUT W42669                                                    
016200     ACCEPT DAGENS-DATUM       FROM DATE                                  
016500                                                                          
016600     MOVE +0                   TO CHKP-ANT                                
016601                                  POST-ANT                                
016602     MOVE '6021'               TO W-IDHTYP                                
016603     MOVE LOW-VALUE            TO NYCKEL-VALFRI                           
016610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     .                                                                    
017100     EJECT                                                                
017110 B-LAS-FRAM-TILL-CHKPOINT SECTION.                                        
017120                                                                          
017130     PERFORM UNTIL END-OF-W42669 OR                                       
017140                       POST-ANT = 6022-KVPOST                             
017150         ADD +1          TO POST-ANT                                      
017160         PERFORM S01-LAES-W42669                                          
017170     END-PERFORM                                                          
017180                                                                          
017190     IF END-OF-W42669                                                     
017191       MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                            
017192                     TO FELTEXT                                           
017193       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017194     END-IF                                                               
017195     .                                                                    
017196     EJECT                                                                
017197 C-BEHANDLA-INDATA SECTION.                                               
017200     SKIP2                                                                
017201     ADD +1          TO POST-ANT                                          
017202                        CHKP-ANT                                          
017203                                                                          
017204     IF CHKP-ANT > CHKP-MAX                                               
017205       PERFORM CAA-TAG-CHECKPOINT                                         
017206       MOVE +0               TO CHKP-ANT                                  
017207     END-IF                                                               
017208                                                                          
017209     MOVE IN-IDLOPNRM        TO W-IDLOPNRM                                
017210     PERFORM IMS-GET-W6UPFA01                                             
017211     IF SEGMENT-FINNS                                                     
017212       PERFORM IMS-DLET-W6UPFA                                            
017213     END-IF                                                               
017220     .                                                                    
017221     EJECT                                                                
017222 CAA-TAG-CHECKPOINT SECTION.                                              
017223*********** UPPDATERAR ÅTERSTARTNINGSREG                                  
017224     PERFORM IMS-LAS-ATERSTART                                            
017225     MOVE POST-ANT              TO 6022-KVPOST                            
017226     ACCEPT 6022-TIUPPDAT       FROM DATE                                 
017227     ACCEPT 6022-TIUPPTID       FROM TIME                                 
017228     PERFORM IMS-REPL-ATERSTART                                           
017229     PERFORM IMS-CHECKPOINT                                               
017240     .                                                                    
017241     EJECT                                                                
017250 Z-FINIT SECTION.                                                         
017701     SKIP2                                                                
017710     CLOSE W42669                                                         
017901******* NOLLA ÅTERSTARTSINFO                                              
017902     PERFORM IMS-LAS-ATERSTART                                            
017903                                                                          
017904     MOVE +0                  TO 6022-KVPOST                              
017905     ACCEPT 6022-TIUPPDAT       FROM DATE                                 
017906     ACCEPT 6022-TIUPPTID       FROM TIME                                 
017907                                                                          
017909     PERFORM IMS-REPL-ATERSTART                                           
017910                                                                          
017911     MOVE 'S' TO POSTSUM-OPKOD                                            
017920     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018201     EJECT                                                                
018202 S01-LAES-W42669  SECTION.                                                
018203     SKIP2                                                                
018204     READ W42669 INTO IN-AREA                                             
018205     AT END                                                               
018207        MOVE JA TO W42669-EOF-SW                                          
018208                                                                          
018209     NOT AT END                                                           
018210        MOVE 'W42669' TO POSTSUM-FDNAMN                                   
018211        MOVE 'W42669D1' TO POSTSUM-DDNAMN2                                
018213        CALL POSTSUM USING POSTSUM-PARM                                   
018214                                                                          
018216     END-READ                                                             
018220     .                                                                    
018500     EJECT                                                                
019800* --- IMS SEKTIONER ---                                                   
019900                                                                          
020002 IMS-GET-W6UPFA01 SECTION.                                                
020003                                                                          
020004     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
020005          DELIMITED BY SIZE INTO SSA1                                     
020006     MOVE '  GE' TO GODK-STATUSKODER                                      
020007     CALL CBLTDLI USING GHU UPFA-PCB DLI-IO-AREA-UPFA SSA1                
020008     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
020009     PERFORM IMS-STATUSKONTROLL                                           
020010     .                                                                    
020011     EJECT                                                                
020012 IMS-DLET-W6UPFA SECTION.                                                 
020013                                                                          
020014     MOVE '  ' TO GODK-STATUSKODER                                        
020015     CALL CBLTDLI USING DLET UPFA-PCB DLI-IO-AREA-UPFA                    
020016     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
020017     PERFORM IMS-STATUSKONTROLL                                           
020020     .                                                                    
020100     EJECT                                                                
020200 IMS-RESTART SECTION.                                                     
020300                                                                          
020400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020500     MOVE '  ' TO GODK-STATUSKODER                                        
020600     CALL CBLTDLI USING XRST MSG-PCB                                      
020700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020800                        CHKP-AREA-LENGTH CHKP-AREA                        
020900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021000     PERFORM IMS-STATUSKONTROLL                                           
021100     .                                                                    
021200     EJECT                                                                
021300 IMS-CHECKPOINT SECTION.                                                  
021400                                                                          
021500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021600     MOVE '  XD' TO GODK-STATUSKODER                                      
021700     CALL CBLTDLI USING CHKP MSG-PCB                                      
021800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021900                        CHKP-AREA-LENGTH CHKP-AREA                        
022000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022100     PERFORM IMS-STATUSKONTROLL                                           
022200                                                                          
022300     IF IMS-EJ-OK                                                         
022400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022500       DISPLAY FELTEXT                                                    
022600       CALL FELLOG                                                        
022700     END-IF                                                               
022800     .                                                                    
022900     EJECT                                                                
022910 IMS-LAS-ATERSTART SECTION.                                               
022920                                                                          
022930     MOVE '6021'             TO W-IDHTYP                                  
022931     MOVE LOW-VALUE          TO NYCKEL-VALFRI                             
022932     STRING 'W6CKPD01(W6GXKEY  =' W-IDHTYP-X ')'                          
022933                    DELIMITED BY SIZE INTO SSA1                           
022934     MOVE 'W6CKPD11 '        TO SSA2                                      
022935     MOVE '  '               TO GODK-STATUSKODER                          
022936     CALL CBLTDLI USING GHU CKPD-PCB DLI-IO-AREA-CKPD SSA1 SSA2           
022939     MOVE CKPD-STATUS-CODE TO STATUS-WS                                   
022940     PERFORM IMS-STATUSKONTROLL                                           
022997     .                                                                    
022998     EJECT                                                                
022999 IMS-REPL-ATERSTART SECTION.                                              
023000                                                                          
023006     MOVE '  '               TO GODK-STATUSKODER                          
023007     CALL CBLTDLI USING REPL CKPD-PCB DLI-IO-AREA-CKPD                    
023008     MOVE CKPD-STATUS-CODE TO STATUS-WS                                   
023009     PERFORM IMS-STATUSKONTROLL                                           
023010     .                                                                    
023011     EJECT                                                                
023020 IMS-STATUSKONTROLL SECTION.                                              
023100                                                                          
023200     SET STATUS-IX TO 1                                                   
023300     SEARCH GODK-STATUS                                                   
023400       AT END                                                             
023500         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
023600         DISPLAY FELTEXT                                                  
023700         CALL FELLOG                                                      
023800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023900         CONTINUE                                                         
024000     END-SEARCH                                                           
024100     .                                                                    
