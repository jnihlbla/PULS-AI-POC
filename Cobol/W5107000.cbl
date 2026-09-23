001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W5107000.                                                
001200 AUTHOR.         MARKUS ASPFJÄLL.                                         
001300 DATE-WRITTEN.   98/09/01.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER NED HELA WDH5/PEDALS KONTERINGSBAS                         
001800*                                                                         
001910*        PROGRAMMET LÄSER      WLEKKA (WDH5)                              
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- UTFIL-WDH5-POSTER                                          
003210     SELECT W51070                     ASSIGN TO W51070D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W51070                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003806*01  POST -COPY W51070   -PRE UT-  -L.                                    
003810                                                                          
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W5107000'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004700     EJECT                                                                
004710 01  WS-KDEKHHT                   PIC X(3).                               
004720 01  WS-BEEKHHT                   PIC X(25).                              
004730 01  WS-KDEKSHT                   PIC X(3).                               
004740 01  WS-BEEKSHT                   PIC X(25).                              
004750 01  WS-KDDOKTYP                  PIC X(2).                               
004760                                                                          
004770                                                                          
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  UT-AREA-START             PIC X(24)   VALUE                          
007203                                 'UT-AREA-START  '.                       
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W51070   -PRE UT-                                         
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-KDEKHHT-X.                                                     
008002         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
008003     03  W-KDEKSHT-X.                                                     
008004         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
008005     03  W-KDEKNIVA-X.                                                    
008006         05  W-KDEKNIVA          PIC X(5)    VALUE SPACE.                 
008007     03  W-IDSYSMOT-X.                                                    
008010         05  W-IDSYSMOT          PIC X(6)    VALUE SPACE.                 
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNKTIONSKODER                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLEKKA'.                      
009802 01  DLI-IO-AREA.                                                         
009810     03  IO-AREA                     PIC X(100) VALUE SPACE.              
009820     03  DLI-IO-WDH501 REDEFINES IO-AREA.                                 
009821*      05  -COPY WDH501                                                   
009822     EJECT                                                                
009823     03  DLI-IO-WDH511 REDEFINES IO-AREA.                                 
009824*      05  -COPY WDH511                                                   
009825     EJECT                                                                
009826     03  DLI-IO-WDH521 REDEFINES IO-AREA.                                 
009827*      05  -COPY WDH521                                                   
009828     EJECT                                                                
009829     03  DLI-IO-WDH531 REDEFINES IO-AREA.                                 
009830*      05  -COPY WDH531                                                   
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300                                                                          
010401                                                                          
010402*01  -COPY W0008  -PRE EKKA-                                              
010410     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010601 PROCEDURE DIVISION  USING EKKA-PCB.                                      
010602 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING EKKA-PCB.                                      
010700                                                                          
010900                                                                          
011000     PERFORM A-INIT                                                       
011100                                                                          
011201     PERFORM IMS-GET-EKKA                                                 
011202     PERFORM UNTIL SEGMENT-SAKNAS                                         
011203       EVALUATE EKKA-SEG-NAME-FB                                          
011204         WHEN 'WDH501'                                                    
011205           MOVE HHT-KDEKHHT TO WS-KDEKHHT                                 
011206           MOVE HHT-BEEKHHT TO WS-BEEKHHT                                 
011209         WHEN 'WDH511'                                                    
011210           MOVE SHT-KDEKSHT TO WS-KDEKSHT                                 
011211           MOVE SHT-BEEKSHT TO WS-BEEKSHT                                 
011214*        WHEN 'WDH521'                                                    
011215*          MOVE NIVA-KDEKNIVA TO UT-KDEKNIVA                              
011216         WHEN 'WDH531'                                                    
011217           MOVE SYST-KDDOKTYP TO WS-KDDOKTYP                              
011218           PERFORM B-FLYTTA-VAERDEN                                       
011219           PERFORM S11-SKRIV-W51070                                       
011220       END-EVALUATE                                                       
011221       PERFORM IMS-GET-EKKA                                               
011230     END-PERFORM                                                          
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012101                                                                          
012110     OPEN OUTPUT W51070                                                   
012200                                                                          
012300     ACCEPT DAGENS-DATUM  FROM DATE                                       
012410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012701 B-FLYTTA-VAERDEN SECTION.                                                
012702     MOVE WS-KDEKHHT    TO UT-KDEKHHT                                     
012703     MOVE WS-BEEKHHT    TO UT-BEEKHHT                                     
012704     MOVE WS-KDEKSHT    TO UT-KDEKSHT                                     
012705     MOVE WS-BEEKSHT    TO UT-BEEKSHT                                     
012706     MOVE WS-KDDOKTYP   TO UT-KDDOKTYP                                    
012721     .                                                                    
012730     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012910     CLOSE W51070                                                         
013001     SKIP2                                                                
013002     MOVE 'S' TO POSTSUM-OPKOD                                            
013010     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013301     EJECT                                                                
013302 S11-SKRIV-W51070 SECTION.                                                
013303                                                                          
013304     WRITE UT-POST FROM UT-AREA                                           
013305                                                                          
013306     MOVE 'UT-'       TO POSTSUM-TRANSTYP                                 
013307     MOVE 'W51070' TO POSTSUM-FDNAMN                                      
013308     MOVE 'W51070D1' TO POSTSUM-DDNAMN2                                   
013309     CALL POSTSUM USING POSTSUM-PARM                                      
013310     .                                                                    
013500     EJECT                                                                
013600 S99-ABEND SECTION.                                                       
013700                                                                          
013801     SKIP2                                                                
013802     MOVE 'S' TO POSTSUM-OPKOD                                            
013810     CALL POSTSUM USING POSTSUM-PARM                                      
013900     CALL ABEND USING RKOD-ABEND                                          
014000     .                                                                    
014100     EJECT                                                                
014200* --- IMS SEKTIONER ---                                                   
014300                                                                          
014401                                                                          
014402 IMS-GET-EKKA   SECTION.                                                  
014403                                                                          
014404     CALL CBLTDLI USING GN EKKA-PCB DLI-IO-AREA                           
014405     MOVE EKKA-STATUS-CODE TO STATUS-WS                                   
014406     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014407     PERFORM IMS-STATUSKONTROLL                                           
014410     .                                                                    
014500     EJECT                                                                
014600 IMS-STATUSKONTROLL SECTION.                                              
014700                                                                          
014800     SET STATUS-IX TO 1                                                   
014900     SEARCH GODK-STATUS                                                   
015000       AT END                                                             
015100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015200           DELIMITED BY SIZE INTO FELTEXT                                 
015300         DISPLAY FELTEXT                                                  
015400         CALL FELLOG                                                      
015500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015600         CONTINUE                                                         
015700     END-SEARCH                                                           
015800     .                                                                    
