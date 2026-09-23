001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W6131300.                                                
001300 AUTHOR.         KENT JEBSEN.                                             
001400 DATE-WRITTEN.   04/09/13.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER NER ALLA WDGX6318-SEGMENT PÅ EN FIL FÖR SENARE             
001810*        BERÄKNING AV ANTALET FÖRPACKNINGAR UTFÖRDA AV SVS.               
001900*                                                                         
002010*        PROGRAMMET LÄSER WDR2 (WDGX6318)                                 
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- NEDLÄSTA WDGX6318 SEGMENT                                  
003310     SELECT W61313                     ASSIGN TO W61313D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W61313                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  POST -COPY WDGX6318 -PRE  UT-  -L.                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6131300'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
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
007302 01  UT-AREA-START               PIC X(24)   VALUE                        
007303                                 'UT-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY WDGX6318     -PRE UT-                                     
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-WDGXKEY-6317-X.                                                
008101         05  W-IDHTYP            PIC X(4)    VALUE '6317'.                
008103         05  W-6317-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
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
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6318'.                    
010002 01  DLI-IO-WDGX6318.                                                     
010010*    03  -COPY WDGX6318                                                   
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE 6318-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING 6318-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING 6318-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011400     PERFORM IMS-GU-WDGX6317                                              
011401     PERFORM IMS-GNP-WDGX6318                                             
011410                                                                          
011420     IF SEGMENT-FINNS                                                     
011500       PERFORM UNTIL SEGMENT-SAKNAS                                       
011700                                                                          
011810         MOVE 6318-WDGX6318 TO UT-6318-WDGX6318                           
011900         PERFORM S11-SKRIV-W61313                                         
012100                                                                          
012200         PERFORM IMS-GNP-WDGX6318                                         
012300       END-PERFORM                                                        
012310     END-IF                                                               
012400                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN OUTPUT W61313                                                   
013500                                                                          
013600     ACCEPT DAGENS-DATUM  FROM DATE                                       
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014100 Z-FINIT SECTION.                                                         
014210     CLOSE W61313                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014601     EJECT                                                                
014602 S11-SKRIV-W61313 SECTION.                                                
014603                                                                          
014604     WRITE UT-POST FROM UT-AREA                                           
014605                                                                          
014606     MOVE 'UT' TO POSTSUM-TRANSTYP                                        
014607     MOVE 'W61313' TO POSTSUM-FDNAMN                                      
014608     MOVE 'W61313D1' TO POSTSUM-DDNAMN2                                   
014609     CALL POSTSUM USING POSTSUM-PARM                                      
014610     .                                                                    
014800     EJECT                                                                
015500* --- IMS SEKTIONER ---                                                   
015600                                                                          
015701     EJECT                                                                
015712 IMS-GU-WDGX6317 SECTION.                                                 
015713                                                                          
015714     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6317-X ')'                    
015715          DELIMITED BY SIZE INTO SSA1                                     
015716     MOVE '  ' TO GODK-STATUSKODER                                        
015717     CALL CBLTDLI USING GU 6318-PCB DLI-IO-WDGX6318 SSA1                  
015718     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
015719     PERFORM IMS-STATUSKONTROLL                                           
015720     .                                                                    
015721     SKIP3                                                                
015722 IMS-GNP-WDGX6318 SECTION.                                                
015723                                                                          
015724     MOVE 'WDGX6318' TO SSA1                                              
015727     MOVE '  GE' TO GODK-STATUSKODER                                      
015728     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
015729     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
015730     PERFORM IMS-STATUSKONTROLL                                           
015731     .                                                                    
015740     SKIP3                                                                
015900 IMS-STATUSKONTROLL SECTION.                                              
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GODK-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO FELTEXT                                 
016600         DISPLAY FELTEXT                                                  
016700         CALL FELLOG                                                      
016800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
