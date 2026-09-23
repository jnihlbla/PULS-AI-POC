001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W4120900.                                                
001400 AUTHOR.         CHRISTINE LINDQVIST.                                     
001500 DATE-WRITTEN.   03/05/19.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET LÄSER EN FIL FRÅN TACDIS                              
002010*        OCH SKAPAR                                                       
002100*        ANNULLATIONSTRANSAR TILL W40254                                  
002200*                                                                         
002300*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
002310*                            WDP8                                         
002320*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
002330*                            WDR4                                         
002340*                                                                         
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- ANNULLATIONSPOSTER                                         
003210     SELECT W41209                     ASSIGN TO W41209D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W41209                                                               
003803     RECORDING       V                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  -COPY W41254      -L.                                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W4120900'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004901                                                                          
004910 77  POST-ANT                    PIC S9(3)   VALUE +0   COMP-3.           
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005110 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005200     SKIP2                                                                
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005701                                                                          
005702 77  W41209-EOF-SW               PIC X       VALUE 'N'.                   
005710     88  END-OF-W41209                       VALUE 'J'.                   
006000     EJECT                                                                
006100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES DAGENS-DATUM.                                       
006300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006600     EJECT                                                                
006610                                                                          
006611 01  W-DATUM                     PIC 9(6)    VALUE ZERO.                  
006612 01  W-TIKLOCK                   PIC 9(8)    VALUE ZERO.                  
006613     EJECT                                                                
006620                                                                          
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
007110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007120     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007502 01  ANNU-AREA-START             PIC X(24)   VALUE                        
007503                                             'ANNU-AREA-START'.           
007504     SKIP2                                                                
007510*01  ANNU-AREA  -COPY W41254                                              
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008100                                                                          
008110 01  FILLER          PIC X(16)   VALUE  'NYCKLAR TILL DLI'.               
008200                                                                          
008201 01  NYCKLAR-TILL-DLI.                                                    
008210     03  W-WDGXKEY-X.                                                     
008220         05  W-IDHTYP            PIC X(4)     VALUE '4579'.               
008221         05  W-IDPGM             PIC X(8)     VALUE 'W4120900'.           
008222         05  W-LOW-VALUE         PIC X(18)    VALUE LOW-VALUE.            
008230                                                                          
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008900     88  IMS-EJ-OK                           VALUE 'XD'.                  
009000     SKIP2                                                                
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010300                                                                          
010310 01  FILLER         PIC X(16) VALUE '4580-IO-AREA'.                       
010320 01  4580-IO-AREA.                                                        
010330*    03  -COPY WDGX4580                                                   
010340                                                                          
010350     EJECT                                                                
010360 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
010370*01  -COPY WMSGKOM                                                        
010380     EJECT                                                                
010390                                                                          
010391 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
010392     SKIP3                                                                
010393*01  -COPY WMSGAREA                                                       
010394     EJECT                                                                
010395*                                                                         
010396*    --- AREOR FÖR W006KOM SUBMODUL                                       
010397*                                                                         
010398 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
010399 01  KOM-IO-AREA.                                                         
010400   03  KOM-AREA                     PIC X(1500) VALUE SPACE.              
010410   03 ARAD REDEFINES KOM-AREA.                                            
010420*    05   -COPY W4I25401 -PRE ARAD-                                       
010430     EJECT                                                                
010500                                                                          
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009   -PRE MSG-                                              
011500     EJECT                                                                
011600 01  DISP-PCB                PIC X.                                       
011601 01  WDP8-PCB                PIC X.                                       
011602                                                                          
011603*01  -COPY W0008  -PRE 4579-                                              
011604     05  FILLER              PIC X.                                       
011605     EJECT                                                                
011606                                                                          
011607 PROCEDURE DIVISION  USING MSG-PCB                                        
011608                           DISP-PCB                                       
011609                           WDP8-PCB                                       
011610                           4579-PCB.                                      
011611 MAIN SECTION.                                                            
011620     ENTRY 'DLITCBL' USING MSG-PCB                                        
011630                           DISP-PCB                                       
011640                           WDP8-PCB                                       
011650                           4579-PCB.                                      
011700                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012010                                                                          
012100     PERFORM IMS-GHU-WDR470                                               
012112                                                                          
012113     IF 4580-KVPOST > +0                                                  
012114        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
012115     ELSE                                                                 
012116        PERFORM S01-LAES-W41209                                           
012117     END-IF                                                               
012118                                                                          
012124     PERFORM UNTIL END-OF-W41209                                          
012125                                                                          
012126        PERFORM C-BEARBETA                                                
012127        PERFORM S01-LAES-W41209                                           
012128                                                                          
012129     END-PERFORM                                                          
013500                                                                          
013600     PERFORM Z-FINIT                                                      
013700                                                                          
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400                                                                          
014500     PERFORM IMS-RESTART                                                  
014701                                                                          
014710     OPEN INPUT W41209                                                    
014720     MOVE +0                   TO POST-ANT                                
014730                                  CHKP-ANT                                
014750                                                                          
014760     MOVE SPACE                TO MSG-AREA                                
014770                                                                          
014780     ACCEPT W-DATUM            FROM DATE                                  
014790     ACCEPT W-TIKLOCK          FROM TIME                                  
015300                                                                          
015410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700     .                                                                    
015900     EJECT                                                                
015910 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
015920                                                                          
015930     PERFORM S01-LAES-W41209                                              
015940                                                                          
015950     PERFORM UNTIL END-OF-W41209  OR                                      
015960                     POST-ANT = 4580-KVPOST                               
015970        PERFORM S01-LAES-W41209                                           
015980        ADD +1           TO POST-ANT                                      
015990     END-PERFORM                                                          
015991                                                                          
015992     IF END-OF-W41209                                                     
015993        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
015994                      TO FELTEXT                                          
015995        DISPLAY FELTEXT                                                   
015996        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
015997     END-IF                                                               
015998     .                                                                    
015999     EJECT                                                                
016000 C-BEARBETA SECTION.                                                      
016001                                                                          
016002     PERFORM CA-SKAPA-ANNULRADTRANS                                       
016003     PERFORM X-TAG-CHECKPOINT                                             
016004     PERFORM IMS-GHU-WDR470                                               
016005     .                                                                    
016006     EJECT                                                                
016007                                                                          
016008 CA-SKAPA-ANNULRADTRANS SECTION.                                          
016009     SKIP2                                                                
016010     MOVE SPACE                  TO KOM-AREA                              
016011                                                                          
016012     COMPUTE MSG-KVLL = LENGTH OF ARAD-MID-W4I25401-CTX + 17              
016013                                                                          
016015     MOVE LOW-VALUE              TO MSG-KDZ1                              
016016     MOVE LOW-VALUE              TO MSG-KDZ2                              
016017     MOVE 'W4T254X '             TO MSG-KDTRANS-1                         
016018     MOVE '4254'                 TO MSG-IDTRANS-1                         
016019     MOVE '1'                    TO MSG-KDMFSFOR-1                        
016020     MOVE +54                    TO MSG-KOM-KVLL                          
016021     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
016022     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
016023     MOVE 'W4I25401'             TO MSG-KOM-IDCPYTXT                      
016024     MOVE 'W41209 '              TO MSG-KOM-IDSNDNOD                      
016025     MOVE 'W4120900'             TO MSG-KOM-IDSNDJOB                      
016026     MOVE W-DATUM                TO MSG-KOM-TIREGDAT                      
016027     ADD +1                      TO W-TIKLOCK                             
016028     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
016029     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
016030                                                                          
016031     PERFORM CAA-SKAPA-ANNULRADTRANS                                      
016032                                                                          
016033     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
016034     CALL W006KOM USING MSG-PCB                                           
016035                        DISP-PCB                                          
016036                        WDP8-PCB                                          
016037                        MSG-KOM-WMSGKOM                                   
016038                        MSG-IO-AREA                                       
016039                                                                          
016040     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
016041*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
016042*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
016043        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W41209 '                    
016044                      TO FELTEXT                                          
016045        DISPLAY FELTEXT                                                   
016046        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
016047     END-IF                                                               
016048                                                                          
016049     MOVE SPACE TO KOM-AREA                                               
016050     .                                                                    
016051     EJECT                                                                
016052                                                                          
016053 CAA-SKAPA-ANNULRADTRANS SECTION.                                         
016054                                                                          
016055     MOVE ANNU-IDSYSTEM          TO ARAD-MID-IDSYSTEM                     
016056     MOVE ANNU-IDDISTR           TO ARAD-MID-IDDISTR                      
016057     MOVE ANNU-IDKUNDNR          TO ARAD-MID-IDKUNDNR                     
016058     MOVE ANNU-IDORDNR           TO ARAD-MID-IDORDNR                      
016059     MOVE 'J'                    TO ARAD-MID-FLSLUT                       
016060     MOVE ANNU-IDARTNR           TO ARAD-MID-IDARTNR                      
016061     MOVE ANNU-KVBEART           TO ARAD-MID-KVBEART                      
016062     MOVE ANNU-IDDC              TO ARAD-MID-IDDC                         
016063     .                                                                    
016064     EJECT                                                                
016065                                                                          
016070 Z-FINIT SECTION.                                                         
016100                                                                          
016501                                                                          
016510     CLOSE W41209                                                         
016701     SKIP2                                                                
016702                                                                          
016703*    NOLLA ÅTERSTARTINFORMATIONEN                                         
016705     MOVE +0         TO 4580-KVPOST                                       
016706     ACCEPT 4580-TIUPPDAT FROM DATE                                       
016707     ACCEPT 4580-TIUPPTID FROM TIME                                       
016708                                                                          
016709     PERFORM IMS-REPL-WDR470                                              
016710                                                                          
016711     MOVE 'S' TO POSTSUM-OPKOD                                            
016720     CALL POSTSUM USING POSTSUM-PARM                                      
016900     .                                                                    
017001     EJECT                                                                
017002 S01-LAES-W41209  SECTION.                                                
017003     SKIP2                                                                
017004     READ W41209 INTO ANNU-AREA                                           
017005     AT END                                                               
017007        SET END-OF-W41209 TO TRUE                                         
017008                                                                          
017009     NOT AT END                                                           
017010        MOVE 'W41209' TO POSTSUM-FDNAMN                                   
017011        MOVE 'W41209D1' TO POSTSUM-DDNAMN2                                
017012        MOVE ANNU-IDPTYP TO POSTSUM-TRANSTYP                              
017013        CALL POSTSUM USING POSTSUM-PARM                                   
017014                                                                          
017016     END-READ                                                             
017020     .                                                                    
017300     EJECT                                                                
017400 X-TAG-CHECKPOINT   SECTION.                                              
017500                                                                          
018320*    UPPDATERA ÅTERSTARTREGISTRET                                         
018340     ADD +1          TO 4580-KVPOST                                       
018350     ACCEPT 4580-TIUPPDAT FROM DATE                                       
018360     ACCEPT 4580-TIUPPTID FROM TIME                                       
018370                                                                          
018380     PERFORM IMS-REPL-WDR470                                              
018390                                                                          
018391*    TAG CHECKPOINT                                                       
018392     PERFORM IMS-CHECKPOINT                                               
018393     .                                                                    
018394     EJECT                                                                
018600* --- IMS SEKTIONER ---                                                   
018700                                                                          
018900     EJECT                                                                
019000 IMS-RESTART SECTION.                                                     
019100     SKIP2                                                                
019200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019300     MOVE '  ' TO GODK-STATUSKODER                                        
019400     CALL CBLTDLI USING XRST MSG-PCB                                      
019500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019600                        CHKP-AREA-LENGTH CHKP-AREA                        
019700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019800     PERFORM IMS-STATUSKONTROLL                                           
019900     .                                                                    
020000     SKIP3                                                                
020100 IMS-CHECKPOINT SECTION.                                                  
020200     SKIP2                                                                
020300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020400     MOVE '  XD' TO GODK-STATUSKODER                                      
020500     CALL CBLTDLI USING CHKP MSG-PCB                                      
020600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020700                        CHKP-AREA-LENGTH CHKP-AREA                        
020800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020900     PERFORM IMS-STATUSKONTROLL                                           
021000                                                                          
021100     IF IMS-EJ-OK                                                         
021200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021300       DISPLAY FELTEXT                                                    
021400       CALL FELLOG                                                        
021500     END-IF                                                               
021600     .                                                                    
021610                                                                          
021620 IMS-GHU-WDR470    SECTION.                                               
021630     SKIP2                                                                
021670     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
021680                    DELIMITED BY SIZE INTO SSA1                           
021690     MOVE 'WDR470   '    TO SSA2                                          
021691     MOVE '  '           TO GODK-STATUSKODER                              
021692     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
021693     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
021694     PERFORM IMS-STATUSKONTROLL                                           
021695     .                                                                    
021696                                                                          
021712 IMS-REPL-WDR470    SECTION.                                              
021713     SKIP2                                                                
021714     MOVE '  '             TO GODK-STATUSKODER                            
021715     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
021716     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
021717     PERFORM IMS-STATUSKONTROLL                                           
021718     .                                                                    
021719                                                                          
021720     EJECT                                                                
021800 IMS-STATUSKONTROLL SECTION.                                              
021900     SKIP2                                                                
022000     SET STATUS-IX TO 1                                                   
022100     SEARCH GODK-STATUS                                                   
022200       AT END                                                             
022300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022400           DELIMITED BY SIZE INTO FELTEXT                                 
022500         DISPLAY FELTEXT                                                  
022600         CALL FELLOG                                                      
022700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022800         CONTINUE                                                         
022900     END-SEARCH                                                           
023000     .                                                                    
