001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W6153000.                                                
001200 AUTHOR.         TOMMIE JIVARP.                                           
001300 DATE-WRITTEN.   97/12/11.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PROGRAMMET SKRIVER EN UTFIL MED LAGERPLATSINFO IFRÅN             
001800*        WDJ8 OCH FREKVENSKOD - PERIODBEHOV IFRÅN WDR2                    
001900*                                                                         
001910*        PROGRAMMET LÄSER      WLLOCA (WDJ8)                              
001920*                              WL6313 (WDR2)                              
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
003202*          --- UTFIL .........                                            
003210     SELECT W61530                     ASSIGN TO W61530D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W61530                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  POST -COPY W61530 -PRE  UT1-  -L.                                    
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W6153000'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600*01  -COPY WWDCKONS                                                       
004610                                                                          
004700     EJECT                                                                
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
006001                                                                          
006010*    --- MINNESTABELL FÖR DATA IFRÅN WDR2                                 
006020 01  FREKVENSTABELL.                                                      
006032     03 FREKV-TAB OCCURS 30.                                              
006042        05  TAB-KDFREQ    PIC X(2)         VALUE SPACE.                   
006050        05  TAB-KVPB-FOM  PIC S9(6)V9(1) COMP-3                           
006051                                           VALUE ZERO.                    
006052        05  TAB-KVPB-TOM  PIC S9(6)V9(1) COMP-3                           
006053                                           VALUE ZERO.                    
006054        05  TAB-RELOCFAC  PIC 9V9(2)       VALUE ZERO.                    
006070                                                                          
006080*    --- DC/ FREKVENSINDEX                                                
006091 01  FREKV-IX                    PIC S9(9)   VALUE ZERO.                  
006100                                                                          
006110*    --- PARAMETRAR TILL ABEND                                            
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
007202 01  UT1-AREA-START              PIC X(24)   VALUE                        
007203                                 'UT1-AREA-START  '.                      
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W61530     -PRE UT1-                                      
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007830                                                                          
007900 01  NYCKLAR-TILL-DLI.                                                    
008000     03  W-WDGXKEY-6313-X.                                                
008001         05  W-IDHTYP            PIC X(4)    VALUE '6313'.                
008002         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008003         05  W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
008004                                                                          
008005     03  W-WDGXKEY-6314-X.                                                
008006         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
008007                                                                          
008008     03  W-WDJ801KY-X.                                                    
008010         05  W-WDJ801KY          PIC X(11)   VALUE SPACE.                 
008100     SKIP2                                                                
008140                                                                          
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GB'                   
008510                                                   'GE'.                  
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
009800                                                                          
009801 01  FILLER         PIC X(30) VALUE 'WL631301-AREA'.                      
009810 01  WL631301-AREA.                                                       
009820*    03  -COPY WDGX6313                                                   
009821 01  FILLER         PIC X(23) VALUE 'WL631311-AREA'.                      
009822 01  WL631311-AREA.                                                       
009823*    03  -COPY WDGX6314                                                   
009824 01  FILLER         PIC X(34) VALUE 'DLI-IO-WLLOCA'.                      
009830 01  DLI-IO-WLLOCA.                                                       
009840*    03  -COPY WDJ801                                                     
010100     EJECT                                                                
010110                                                                          
010200 LINKAGE SECTION.                                                         
010300                                                                          
010408*01  -COPY W0008  -PRE LOCA-                                              
010410     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010510*01  -COPY W0008  -PRE 6313-                                              
010520     05  FILLER                  PIC X.                                   
010530     EJECT                                                                
010600                                                                          
010603 PROCEDURE DIVISION  USING LOCA-PCB 6313-PCB.                             
010604 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING LOCA-PCB 6313-PCB.                             
010700                                                                          
010900                                                                          
011000     PERFORM A-INIT                                                       
011100                                                                          
011204     PERFORM B-LAES-FLYTTA-TILL-FREKV-TAB                                 
011205     PERFORM IMS-GN-LOCA                                                  
011206     PERFORM UNTIL SEGMENT-SAKNAS                                         
011207       EVALUATE LOCA-SEG-NAME-FB                                          
011208         WHEN 'WDJ801'                                                    
011209           IF LOC-IDDC = WC-CDC-SE                                        
011210              PERFORM C-HAEMTA-FREKVENSTABELL-INFO                        
011211              PERFORM D-FLYTTA-TILL-UT1-AREA                              
011212              PERFORM S11-SKRIV-W61530                                    
011213           END-IF                                                         
011214       END-EVALUATE                                                       
011215       PERFORM IMS-GN-LOCA                                                
011220     END-PERFORM                                                          
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011810                                                                          
011900 A-INIT SECTION.                                                          
012101                                                                          
012110     MOVE WC-CDC-SE TO W-IDDC                                             
012120                                                                          
012130     OPEN OUTPUT W61530                                                   
012200                                                                          
012300     ACCEPT DAGENS-DATUM  FROM DATE                                       
012410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012701                                                                          
012702 B-LAES-FLYTTA-TILL-FREKV-TAB SECTION.                                    
012703                                                                          
012706     MOVE +1 TO FREKV-IX                                                  
012710     MOVE WC-CDC-SE TO W-IDDC                                             
012711                                                                          
012713     PERFORM IMS-GU-WL631301                                              
012714     IF SEGMENT-FINNS                                                     
012715        PERFORM IMS-GNP-WL631311                                          
012716        PERFORM UNTIL SEGMENT-SAKNAS OR FREKV-IX > 30                     
012718           MOVE 6314-KDFREQ   TO TAB-KDFREQ   (FREKV-IX)                  
012719           MOVE 6314-KVPB-FOM TO TAB-KVPB-FOM (FREKV-IX)                  
012720           MOVE 6314-KVPB-TOM TO TAB-KVPB-TOM (FREKV-IX)                  
012721           MOVE 6314-RELOCFAC TO TAB-RELOCFAC (FREKV-IX)                  
012722           ADD +1 TO FREKV-IX                                             
012723           PERFORM IMS-GNP-WL631311                                       
012724        END-PERFORM                                                       
012725        MOVE +1 TO FREKV-IX                                               
012726     END-IF                                                               
012729     .                                                                    
012730     EJECT                                                                
012731                                                                          
012732 C-HAEMTA-FREKVENSTABELL-INFO SECTION.                                    
012733                                                                          
012735     MOVE +1 TO FREKV-IX                                                  
012738     PERFORM UNTIL FREKV-IX > 30                                          
012739        IF TAB-KDFREQ (FREKV-IX) = LOC-KDFREQ                             
012741           MOVE TAB-KVPB-FOM (FREKV-IX) TO UT1-KVPB-FOM                   
012750           MOVE TAB-KVPB-TOM (FREKV-IX) TO UT1-KVPB-TOM                   
012760           MOVE TAB-RELOCFAC (FREKV-IX) TO UT1-RELOCFAC                   
012761           ADD +1 TO FREKV-IX                                             
012762        ELSE                                                              
012763           ADD +1 TO FREKV-IX                                             
012764        END-IF                                                            
012765     END-PERFORM                                                          
012770     .                                                                    
012771     EJECT                                                                
012772                                                                          
012773 D-FLYTTA-TILL-UT1-AREA SECTION.                                          
012774                                                                          
012775     MOVE LOC-IDDC      TO UT1-IDDC                                       
012776     MOVE LOC-ADLAGOMR  TO UT1-ADLAGOMR                                   
012777     MOVE LOC-ADGANG    TO UT1-ADGANG                                     
012778     MOVE LOC-ADPLATS   TO UT1-ADPLATS                                    
012779     MOVE LOC-KDFREQ    TO UT1-KDFREQ                                     
012780     MOVE LOC-KDSTOR    TO UT1-KDSTOR                                     
012790     .                                                                    
012791     EJECT                                                                
012792                                                                          
012800 Z-FINIT SECTION.                                                         
012910     CLOSE W61530                                                         
013001     SKIP2                                                                
013002     MOVE 'S' TO POSTSUM-OPKOD                                            
013010     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013301     EJECT                                                                
013302                                                                          
013303 S11-SKRIV-W61530 SECTION.                                                
013304                                                                          
013305     WRITE UT1-POST FROM UT1-AREA                                         
013306                                                                          
013307     MOVE SPACE TO POSTSUM-TRANSTYP                                       
013308     MOVE 'W61530' TO POSTSUM-FDNAMN                                      
013309     MOVE 'W61530D1' TO POSTSUM-DDNAMN2                                   
013310     CALL POSTSUM USING POSTSUM-PARM                                      
013320     .                                                                    
013500     EJECT                                                                
013510                                                                          
014200* --- IMS SEKTIONER ---                                                   
014402                                                                          
014403 IMS-GU-WL631301 SECTION.                                                 
014404                                                                          
014405     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
014406          DELIMITED BY SIZE INTO SSA1                                     
014410     MOVE '  GE' TO GODK-STATUSKODER                                      
014411     CALL CBLTDLI USING GU 6313-PCB WL631301-AREA SSA1                    
014412     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
014413     PERFORM IMS-STATUSKONTROLL                                           
014414     .                                                                    
014415     SKIP3                                                                
014416                                                                          
014417 IMS-GNP-WL631311 SECTION.                                                
014418                                                                          
014419     MOVE 'WL631311 ' TO SSA1                                             
014424     MOVE '  GE' TO GODK-STATUSKODER                                      
014425     CALL CBLTDLI USING GNP 6313-PCB WL631311-AREA SSA1                   
014426     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
014427     PERFORM IMS-STATUSKONTROLL                                           
014428     .                                                                    
014429     SKIP3                                                                
014430 IMS-GN-LOCA   SECTION.                                                   
014431                                                                          
014432     CALL CBLTDLI USING GN LOCA-PCB DLI-IO-WLLOCA                         
014433     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
014434     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014435     PERFORM IMS-STATUSKONTROLL                                           
014440     .                                                                    
014500     EJECT                                                                
014510                                                                          
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
