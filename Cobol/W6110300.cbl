001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6110300.                                                
001300*AUTHOR.         MÅNS SAMUELSSON.                                         
001400*DATE-WRITTEN.   93/12/30.                                                
001500                                                                          
001600*    REMARKS                                                              
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        ÄNDRAR PRARTSTD PÅ INLEVERANSREGISTRET NÄR                       
002000*        ART.REG HAR BLIVIT ÄNDRAT                                        
002100*                                                                         
002201*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
002210*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W6110300'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  CKP-ANTAL                   PIC S9(3)   COMP-3 VALUE +0.             
004900 77  CKP-MAX                     PIC S9(3)   COMP-3 VALUE +10.            
004901*                                                                         
004902 01  CHKP-VAR.                                                            
004903     03  CHKP-MSG-IO-AREA-LENGTH PIC S9(9)   VALUE +32 COMP SYNC.         
004904     03  CHKP-MSG-IO-AREA        PIC X(32)   VALUE SPACE.                 
004905     03  CHKP-AREA-LENGTH        PIC S9(9)   VALUE +32 COMP SYNC.         
004910     03  CHKP-AREA               PIC X(32)   VALUE SPACE.                 
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-W6D101KY.                                                      
008202         05  W-IDDC              PIC  X(2)        VALUE SPACE.            
008203         05  W-IDLEVNR           PIC  X(5)        VALUE SPACE.            
008204         05  W-IDFS              PIC  X(8)        VALUE SPACE.            
008205         05  W-TIAVIDAT          PIC S9(7) COMP-3 VALUE +0.               
008206     03  W-IDARTNR-X.                                                     
008207         05  W-IDARTNR           PIC S9(9) COMP-3 VALUE +0.               
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008810     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA-1.                                                       
010400     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
010501     SKIP3                                                                
010502     03  W6INLA11 REDEFINES IO-AREA-1.                                    
010503*        05  -COPY W6D111  -PRE INLA-                                     
010504 01  DLI-IO-AREA-2.                                                       
010505     03  IO-AREA-2               PIC X(900)  VALUE SPACE.                 
010506     SKIP3                                                                
010507     SKIP3                                                                
010508     03  WLARTC11 REDEFINES IO-AREA-2.                                    
010510*        05  -COPY WDK611  -PRE ARTC-                                     
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011101     EJECT                                                                
011102*01  -COPY W0009  -PRE MSG-                                               
011104     EJECT                                                                
011108*01  -COPY W0008  -PRE INLA-                                              
011110     05  INLA-PCB-IDDC           PIC  X(2).                               
011111     05  INLA-PCB-IDLEVNR        PIC  X(5).                               
011112     05  INLA-PCB-IDFS           PIC  X(8).                               
011113     05  INLA-PCB-TIAVIDAT       PIC S9(7) COMP-3.                        
011114     EJECT                                                                
011115*01  -COPY W0008  -PRE ARTC-                                              
011116     05  FILLER                  PIC X.                                   
011120     EJECT                                                                
011301 PROCEDURE DIVISION  USING MSG-PCB INLA-PCB ARTC-PCB.                     
011310     ENTRY 'DLITCBL' USING MSG-PCB INLA-PCB ARTC-PCB.                     
011600     SKIP2                                                                
011700     PERFORM IMS-RESTART                                                  
011800     PERFORM IMS-GHN-INLA11                                               
011900     PERFORM UNTIL NOT SEGMENT-FINNS                                      
012000        MOVE INLA-ART-IDARTNR TO W-IDARTNR                                
012100        PERFORM IMS-GET-ARTC11                                            
012200        IF SEGMENT-FINNS                                                  
012300          IF INLA-ART-PRARTSTD = ARTC-CLAG-PRARTSTD                       
012400            CONTINUE                                                      
012500          ELSE                                                            
012510            ADD +1 TO CKP-ANTAL                                           
012600            MOVE ARTC-CLAG-PRARTSTD TO INLA-ART-PRARTSTD                  
012610            PERFORM IMS-REPL-INLA11                                       
012620          END-IF                                                          
012630        END-IF                                                            
012631        IF CKP-ANTAL > CKP-MAX                                            
012632          MOVE INLA-PCB-IDDC    TO W-IDDC                                 
012633          MOVE INLA-PCB-IDLEVNR TO W-IDLEVNR                              
012634          MOVE INLA-PCB-IDFS    TO W-IDFS                                 
012635          MOVE INLA-PCB-TIAVIDAT TO W-TIAVIDAT                            
012636          PERFORM IMS-CHECKPOINT                                          
012637          PERFORM IMS-GU-INLA11                                           
012638          MOVE +0 TO CKP-ANTAL                                            
012639        END-IF                                                            
012640        PERFORM IMS-GHN-INLA11                                            
012700     END-PERFORM                                                          
012800                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
015900* --- IMS SEKTIONER ---                                                   
016000     SKIP3                                                                
016001 IMS-RESTART SECTION.                                                     
016002                                                                          
016003     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
016004     MOVE '  ' TO GODK-STATUSKODER                                        
016005     CALL CBLTDLI USING XRST MSG-PCB                                      
016006                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
016007                        CHKP-AREA-LENGTH CHKP-AREA                        
016008     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
016009     PERFORM IMS-STATUSKONTROLL                                           
016010     .                                                                    
016011     SKIP2                                                                
016012 IMS-CHECKPOINT SECTION.                                                  
016013                                                                          
016014     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
016015     MOVE '  XD' TO GODK-STATUSKODER                                      
016016     CALL CBLTDLI USING CHKP MSG-PCB                                      
016017                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
016018                        CHKP-AREA-LENGTH CHKP-AREA                        
016019     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
016020     PERFORM IMS-STATUSKONTROLL                                           
016021                                                                          
016022     IF IMS-EJ-OK                                                         
016023       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
016024       DISPLAY FELTEXT                                                    
016025       CALL FELLOG                                                        
016030     END-IF                                                               
016100     .                                                                    
016101     EJECT                                                                
016102 IMS-GU-INLA11   SECTION.                                                 
016103     STRING 'W6INLA01(W6D101KY =' W-W6D101KY  ')'                         
016104          DELIMITED BY SIZE INTO SSA1                                     
016105     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
016106          DELIMITED BY SIZE INTO SSA2                                     
016107     MOVE '  ' TO GODK-STATUSKODER                                        
016108     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-1 SSA1                    
016109     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
016110     PERFORM IMS-STATUSKONTROLL                                           
016111     .                                                                    
016112     SKIP3                                                                
016113 IMS-GHN-INLA11   SECTION.                                                
016114     MOVE 'W6INLA11 ' TO SSA1                                             
016115     MOVE '  GEGB' TO GODK-STATUSKODER                                    
016116     CALL CBLTDLI USING GHN INLA-PCB DLI-IO-AREA-1 SSA1                   
016117     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
016118     PERFORM IMS-STATUSKONTROLL                                           
016119     .                                                                    
016120     SKIP3                                                                
016121 IMS-REPL-INLA11 SECTION.                                                 
016122                                                                          
016123     MOVE '  ' TO GODK-STATUSKODER                                        
016124     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA-1                       
016125     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
016126     PERFORM IMS-STATUSKONTROLL                                           
016127     .                                                                    
016128     EJECT                                                                
016129 IMS-GET-ARTC11    SECTION.                                               
016130     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
016131          DELIMITED BY SIZE INTO SSA1                                     
016132     MOVE 'WLARTC11 ' TO SSA2                                             
016133     MOVE '  GE' TO GODK-STATUSKODER                                      
016134     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA-2 SSA1 SSA2              
016135     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016136     PERFORM IMS-STATUSKONTROLL                                           
016140     .                                                                    
016200     EJECT                                                                
016300 IMS-STATUSKONTROLL SECTION.                                              
016400     SKIP2                                                                
016500     SET STATUS-IX TO 1                                                   
016600     SEARCH GODK-STATUS                                                   
016700       AT END                                                             
016800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016900         DISPLAY FELTEXT                                                  
017000         CALL FELLOG                                                      
017100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017200         CONTINUE                                                         
017300     END-SEARCH                                                           
017400     .                                                                    
