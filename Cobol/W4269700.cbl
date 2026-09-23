001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4269700.                                                
001300*AUTHOR.         INGER NILSSON.                                           
001400*DATE-WRITTEN.   92/10/15.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LADDA W6D2                                                       
002000*                                                                         
002110*        PROGRAMMET UPPDATERAR W6KVAH (W6D2)                              
002120*        PROGRAMMET LÄSER      W6PROA (W6G1)                              
002130*                              W6INLE (WDL2)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- ARTIKLAR FÖR UPPDATERING PÅ W6D2                           
003410     SELECT W42697                     ASSIGN TO W42697D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W42697                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004010 01  INPOST.                                                              
004020*   03 -COPY W4269702 -PRE IN-                                            
004110     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004302*    -- CHECKED BY WY2000                                                 
004303*    ---- ARBETSVARIABLER                                                 
004304*                                                                         
004305 01  MAX-TAL                     PIC S9(09) COMP.                         
004306 01  SLUMP-TAL                   PIC S9(09) COMP.                         
004310                                                                          
004320 77  W-IDPTYP                    PIC X(3)    VALUE 'R32'.                 
004330 77  W-IDLEVNR                   PIC X(5)    VALUE SPACE.                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4269700'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004801                                                                          
004802 77  TIUPPDAT-SW                 PIC X       VALUE 'N'.                   
004810     88  TIUPPDAT-FOUND                      VALUE 'J'.                   
004820                                                                          
004830 77  W42697-EOF-SW               PIC X       VALUE 'N'.                   
004840     88  END-OF-W42697                       VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006120     03  WRANDOM                 PIC X(8)    VALUE 'WRANDOM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-IDARTNR-X.                                                     
008102         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008103*    03  W-IDLEVNR-X.                                                     
008110*        05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
008111     03  W-W6GX-6101-KEY-X.                                               
008112         05 W-IDHTYP-6101        PIC X(04)  VALUE '6101'.                 
008113         05 FILLER               PIC X(26)  VALUE LOW-VALUE.              
008114     03  W-W6GX-6102-KEY-X.                                               
008115         05 W-IDPROVPL-X.                                                 
008116           07 W-IDPROVPL         PIC 9(1).                                
008117         05 W-KDPROVPL-X.                                                 
008120           07 W-KDPROVPL         PIC X(1)   VALUE 'N'.                    
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009310 01  SSA3                        PIC X(96).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009701 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009723 01  DLI-IO-AREA-KVAH01.                                                  
009724     03  W6KVAH01.                                                        
009725*        05  -COPY W6D201                                                 
009726     SKIP3                                                                
009727 01  DLI-IO-AREA-KVAH12.                                                  
009728     03  W6KVAH12.                                                        
009729*        05  -COPY W6D212                                                 
009730     SKIP3                                                                
009735 01  DLI-IO-AREA-PROA11.                                                  
009736     03  W6PROA11.                                                        
009737*        05  -COPY W6GX6102                                               
009738     SKIP3                                                                
009739 01  DLI-IO-AREA-INLE21.                                                  
009740     03  WLINLE21.                                                        
009741*        05  -COPY WDL221                                                 
009742     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011001     EJECT                                                                
011002*01  -COPY W0008  -PRE KVAH-                                              
011010     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011206 PROCEDURE DIVISION  USING KVAH-PCB.                                      
011210     ENTRY 'DLITCBL' USING KVAH-PCB.                                      
011300                                                                          
011500     SKIP2                                                                
011600     PERFORM A-INIT                                                       
011710     PERFORM S01-LAES-W42697                                              
011800     PERFORM UNTIL END-OF-W42697                                          
011810                                                                          
011900       MOVE IN-IDARTNR      TO ART-IDARTNR                                
012000       MOVE IN-ADKVAULG     TO ART-ADKVAULG                               
012100       MOVE IN-KDKVAKTL     TO ART-KDKVAKTL                               
012200       PERFORM IMS-ISRT-W6KVAH01                                          
012300                                                                          
012301       MOVE IN-IDARTNR      TO W-IDARTNR                                  
012310       MOVE IN-IDLEVNR      TO LEV-IDLEVNR                                
012320       MOVE NEJ             TO LEV-FLKVARED                               
012330       MOVE NEJ             TO LEV-FLKVASAK                               
012340       MOVE JA              TO LEV-FLSKPSAK                               
012350       MOVE JA              TO LEV-FLUPG                                  
012360       MOVE '1'             TO LEV-KDKVASAK                               
012370       MOVE '1'             TO LEV-KDKVAUP                                
012396       MOVE IN-KVSKPLOT-PRI TO LEV-KVSKPLOT-PRI                           
012406       MOVE IN-KVSKPLOT-SEK TO LEV-KVSKPLOT-SEK                           
012422       MOVE IN-TIUPPDAT     TO LEV-TIUPPDAT                               
012425       MOVE DAGENS-DATUM    TO LEV-TIKVASAK                               
012426       MOVE DAGENS-DATUM    TO LEV-TIUPG                                  
012430       PERFORM IMS-ISRT-W6KVAH12                                          
012500                                                                          
012510       PERFORM S01-LAES-W42697                                            
012600     END-PERFORM                                                          
012700                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013601                                                                          
013610     OPEN INPUT  W42697                                                   
013800     SKIP2                                                                
013900     ACCEPT DAGENS-DATUM  FROM DATE                                       
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014510     CLOSE W42697                                                         
014601     SKIP2                                                                
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014801     EJECT                                                                
014802 S01-LAES-W42697  SECTION.                                                
014803     SKIP2                                                                
014804     READ W42697                                                          
014805     AT END                                                               
014807        SET END-OF-W42697 TO TRUE                                         
014809                                                                          
014810     NOT AT END                                                           
014811        MOVE 'W42697' TO POSTSUM-FDNAMN                                   
014812        MOVE 'W42697D1' TO POSTSUM-DDNAMN2                                
014813        MOVE 'KVAL'    TO POSTSUM-TRANSTYP                                
014814        CALL POSTSUM USING POSTSUM-PARM                                   
014815     END-READ                                                             
014820     .                                                                    
015100     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016011 IMS-ISRT-W6KVAH01 SECTION.                                               
016012                                                                          
016013     MOVE 'W6KVAH01 ' TO SSA1                                             
016014     MOVE '  ' TO GODK-STATUSKODER                                        
016015     CALL CBLTDLI USING ISRT KVAH-PCB DLI-IO-AREA-KVAH01 SSA1             
016016     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
016017     PERFORM IMS-STATUSKONTROLL                                           
016018     .                                                                    
016019     SKIP2                                                                
016029 IMS-ISRT-W6KVAH12 SECTION.                                               
016030                                                                          
016031     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
016032          DELIMITED BY SIZE INTO SSA1                                     
016033     MOVE 'W6KVAH12 ' TO SSA2                                             
016034     MOVE '  ' TO GODK-STATUSKODER                                        
016035     CALL CBLTDLI USING ISRT KVAH-PCB DLI-IO-AREA-KVAH12 SSA1 SSA2        
016036     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
016037     PERFORM IMS-STATUSKONTROLL                                           
016040     .                                                                    
016211     SKIP3                                                                
016220 IMS-STATUSKONTROLL SECTION.                                              
016300     SKIP2                                                                
016400     SET STATUS-IX TO 1                                                   
016500     SEARCH GODK-STATUS                                                   
016600       AT END                                                             
016700         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016800         DISPLAY FELTEXT                                                  
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
