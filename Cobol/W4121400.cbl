001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W4121400.                                                
001400*AUTHOR.         LASSI OLGRENER.                                          
001500*DATE-WRITTEN.   93/12/07.                                                
001600                                                                          
001700*    REMARKS                                                              
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        RENSNING VORKÖ - WL454111                                        
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WL4541 (WDR4)                              
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
003501     SKIP2                                                                
003502*          --- RENS.POSTER                                                
003510     SELECT W41214                     ASSIGN TO W41214D1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W41214                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004110*01  -COPY W41214      -L.                                                
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4121400'.            
004600 01  CHKP-VAR.                                                            
004700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005200 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001                                                                          
006002 77  W41214-EOF-SW               PIC X       VALUE 'N'.                   
006010     88  END-OF-W41214                       VALUE 'J'.                   
006300     EJECT                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007410     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007501     EJECT                                                                
007502*    --- PARAMETRAR TILL POSTSUM                                          
007503*                                                                         
007510*01  -COPY W0005   -PRE  POSTSUM-                                         
007801     EJECT                                                                
007802 01  IN-AREA-START               PIC X(24)   VALUE                        
007803                                             'IN-AREA-START'.             
007804     SKIP2                                                                
007805                                                                          
007810*01  AREA -COPY W41214     -PRE IN-                                       
007900*                                                                         
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  NYCKLAR-TILL-DLI.                                                    
008301     03  W-WDGXKEY-ROT-X.                                                 
008302         05  FILLER              PIC X(4)     VALUE '4541'.               
008303         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
008304     SKIP2                                                                
008305     03  W-WDGXKEY-X.                                                     
008310         05  W-WDGXKEY           PIC X(16)    VALUE SPACE.                
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010400*    ---  DLI INPUT-OUTPUT AREA                                           
010500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010600     SKIP3                                                                
010700 01  DLI-IO-AREA.                                                         
010903*    03  -COPY WDGX4542                                                   
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200*01  -COPY W0009   -PRE MSG-                                              
012301     EJECT                                                                
012302*01  -COPY W0008  -PRE 4541-                                              
012310     05  FILLER                  PIC X.                                   
012600     EJECT                                                                
012701 PROCEDURE DIVISION  USING MSG-PCB 4541-PCB.                              
012710     ENTRY 'DLITCBL' USING MSG-PCB 4541-PCB.                              
012800                                                                          
013100     PERFORM A-INIT                                                       
013210     PERFORM S01-LAES-W41214                                              
013301     PERFORM UNTIL END-OF-W41214                                          
013302                                                                          
013400       IF CHKP-ANT > CHKP-MAX                                             
013500         PERFORM X-TAG-CHECKPOINT                                         
013600       END-IF                                                             
013700                                                                          
013701       PERFORM IMS-GET-454101                                             
013702                                                                          
013710       MOVE IN-AREA TO W-WDGXKEY                                          
013720       PERFORM IMS-GET-454111                                             
013721       IF SEGMENT-FINNS                                                   
013722         PERFORM IMS-DLET-4541                                            
013723         ADD 1 TO CHKP-ANT                                                
013724       END-IF                                                             
014200                                                                          
014310       PERFORM S01-LAES-W41214                                            
014400     END-PERFORM                                                          
014600                                                                          
014700     PERFORM Z-FINIT                                                      
014800                                                                          
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 A-INIT SECTION.                                                          
015500                                                                          
015600     PERFORM IMS-RESTART                                                  
015801                                                                          
015810     OPEN INPUT W41214                                                    
016400                                                                          
016510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016800     .                                                                    
017000     EJECT                                                                
017100 Z-FINIT SECTION.                                                         
017200                                                                          
017610     CLOSE W41214                                                         
017801     SKIP2                                                                
017802     MOVE 'S' TO POSTSUM-OPKOD                                            
017810     CALL POSTSUM USING POSTSUM-PARM                                      
018000     .                                                                    
018101     EJECT                                                                
018102 S01-LAES-W41214  SECTION.                                                
018103     SKIP2                                                                
018104     READ W41214 INTO IN-AREA                                             
018105     AT END                                                               
018107        SET END-OF-W41214 TO TRUE                                         
018108                                                                          
018109     NOT AT END                                                           
018110        MOVE 'W41214' TO POSTSUM-FDNAMN                                   
018111        MOVE 'W41214D1' TO POSTSUM-DDNAMN2                                
018112        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
018113        CALL POSTSUM USING POSTSUM-PARM                                   
018116     END-READ                                                             
018120     .                                                                    
018400     EJECT                                                                
018500 X-TAG-CHECKPOINT   SECTION.                                              
018600                                                                          
019200     PERFORM IMS-CHECKPOINT                                               
019300     MOVE ZERO TO CHKP-ANT                                                
019500     .                                                                    
019600     EJECT                                                                
019700* --- IMS SEKTIONER ---                                                   
019800     SKIP3                                                                
019902 IMS-GET-454101 SECTION.                                                  
019903     STRING 'WL454101(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
019904          DELIMITED BY SIZE INTO SSA1                                     
019905     MOVE '    ' TO GODK-STATUSKODER                                      
019906     CALL CBLTDLI USING GU 4541-PCB DLI-IO-AREA SSA1                      
019907     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
019908     PERFORM IMS-STATUSKONTROLL                                           
019909     .                                                                    
019910     EJECT                                                                
019911 IMS-GET-454111 SECTION.                                                  
019912     STRING 'WL454111(KY4542   =' W-WDGXKEY-X ')'                         
019913          DELIMITED BY SIZE INTO SSA1                                     
019914     MOVE '  GE' TO GODK-STATUSKODER                                      
019915     CALL CBLTDLI USING GHNP 4541-PCB DLI-IO-AREA SSA1                    
019916     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
019917     PERFORM IMS-STATUSKONTROLL                                           
019918     .                                                                    
019919     SKIP3                                                                
019920 IMS-DLET-4541 SECTION.                                                   
019921                                                                          
019922     MOVE '  ' TO GODK-STATUSKODER                                        
019923     CALL CBLTDLI USING DLET 4541-PCB DLI-IO-AREA                         
019924     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
019925     PERFORM IMS-STATUSKONTROLL                                           
019930     .                                                                    
020000     EJECT                                                                
020100 IMS-RESTART SECTION.                                                     
020200     SKIP2                                                                
020300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020400     MOVE '  ' TO GODK-STATUSKODER                                        
020500     CALL CBLTDLI USING XRST MSG-PCB                                      
020600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020700                        CHKP-AREA-LENGTH CHKP-AREA                        
020800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020900     PERFORM IMS-STATUSKONTROLL                                           
021000     .                                                                    
021100     EJECT                                                                
021200 IMS-CHECKPOINT SECTION.                                                  
021300     SKIP2                                                                
021400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021500     MOVE '  XD' TO GODK-STATUSKODER                                      
021600     CALL CBLTDLI USING CHKP MSG-PCB                                      
021700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021800                        CHKP-AREA-LENGTH CHKP-AREA                        
021900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022000     PERFORM IMS-STATUSKONTROLL                                           
022100                                                                          
022200     IF IMS-EJ-OK                                                         
022300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022400       DISPLAY FELTEXT                                                    
022500       CALL FELLOG                                                        
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 IMS-STATUSKONTROLL SECTION.                                              
023000     SKIP2                                                                
023100     SET STATUS-IX TO 1                                                   
023200     SEARCH GODK-STATUS                                                   
023300       AT END                                                             
023400         MOVE 'IMS BANG..' TO FELTEXT-STR                                 
023500         DISPLAY FELTEXT                                                  
023600         CALL FELLOG                                                      
023700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023800         CONTINUE                                                         
023900     END-SEARCH                                                           
024000     .                                                                    
