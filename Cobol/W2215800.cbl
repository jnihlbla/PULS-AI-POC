000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2215800.                                                
000300 AUTHOR.         TOMMIE JIVARP.                                           
000400 DATE-WRITTEN.   97/10/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LISTNING AV PB-PLAN                                              
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001100*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001200*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001300*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
001400*        PROGRAMMET LÄSER      WL2501 (WDR2)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- ARTIKELDATA WDK6:                                          
002900     SELECT W01160                     ASSIGN TO W22158D1.                
003000     SKIP2                                                                
003100                                                                          
003200*          --- LISTA PÅ ARTIKLAR MED MANUELLT PB SORTERAD PÅ              
003300*          --- ANSKAFFARNUMMER OCH ARTIKELNUMMER. VISAR ÄVEN              
003400*          --- MASKINELLT PB SAMT LEVNR OCH DET MANUELLA PB:TS            
003500*          --- GILTIGT TOM DATUM.                                         
003600     SELECT W22158-001                 ASSIGN TO W22158D2.                
003700     SKIP2                                                                
003800                                                                          
003900*          --- SORTERINGSFIL                                              
004000     SELECT SORTFIL                    ASSIGN TO W22158DS.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W01160                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W01160      -L.                                                
005100     SKIP3                                                                
005200 FD  W22158-001                                                           
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600 01  W22158-001-RAD              PIC X(121).                              
005700     SKIP2                                                                
005800 SD  SORTFIL.                                                             
005900 01  SORT-POST.                                                           
006000                                                                          
006100     03  SORT-IDARTNR           PIC S9(9)      COMP-3.                    
006200     03  SORT-IDANSK            PIC S9(3)      COMP-3.                    
006300     03  SORT-KVPB-PLAN         PIC S9(6)V9(1) COMP-3.                    
006400     03  SORT-DAPBPLAN          PIC  9(8).                                
006500     03  SORT-IDLEVNR           PIC  X(5).                                
006600                                                                          
006700                                                                          
006800     EJECT                                                                
006900                                                                          
007000 WORKING-STORAGE SECTION.                                                 
007100                                                                          
007200                                                                          
007300*    -- CHECKED BY WY2000                                                 
007400 77  IDPGM                       PIC X(8)      VALUE 'W2215800'.          
007500 77  JA                          PIC X         VALUE 'J'.                 
007600 77  NEJ                         PIC X         VALUE 'N'.                 
007700                                                                          
007800                                                                          
007900 77  W01160-EOF-SW               PIC X         VALUE 'N'.                 
008000     88  END-OF-W01160                         VALUE 'J'.                 
008100                                                                          
008200 77  SORTFIL-EOF-SW              PIC X         VALUE 'N'.                 
008300     88  END-OF-SORTFIL                        VALUE 'J'.                 
008400     EJECT                                                                
008500                                                                          
008600 01  ARBETSAREOR.                                                         
008700                                                                          
008800     03  WS-SAVE-IDANSK             PIC S9(3)  VALUE ZERO COMP-3.         
008900     03  WS-FIRST-IDANSK            PIC X(1)   VALUE 'J'.                 
009000                                                                          
009100 01  DAGENS-DATUM                PIC 9(6)      VALUE ZERO.                
009200 01  FILLER REDEFINES DAGENS-DATUM.                                       
009300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009600     EJECT                                                                
009700                                                                          
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900                                                                          
010000     03  ABEND                   PIC X(8)      VALUE 'ABEND'.             
010100     03  CBLTDLI                 PIC X(8)      VALUE 'CBLTDLI '.          
010200     03  FELLOG                  PIC X(8)      VALUE 'FELLOG  '.          
010300     03  POSTSUM                 PIC X(8)      VALUE 'POSTSUM'.           
010400     03  W222PBTO                PIC X(8)      VALUE 'W222PBTO'.          
010500     SKIP2                                                                
010600*    --- PARAMETRAR TILL ABEND                                            
010700                                                                          
010800 77  RKOD-ABEND                  PIC S9(4)     COMP VALUE +0.             
010900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)     COMP VALUE +16.            
011000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)     COMP VALUE +1000.          
011100     SKIP2                                                                
011200                                                                          
011300 01  FELTEXT.                                                             
011400     03  FILLER                  PIC X(8)      VALUE 'FELTEXT'.           
011500     03  FELTEXT-STR             PIC X(72)     VALUE SPACE.               
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL POSTSUM                                          
011800                                                                          
011900*01  -COPY W0005   -PRE  POSTSUM-                                         
012000     EJECT                                                                
012100*01  -COPY W222PBTO                                                       
012200     EJECT                                                                
012300 01  I60-AREA-START              PIC X(24)     VALUE                      
012400                                 'I60-AREA-START  '.                      
012500     SKIP2                                                                
012600                                                                          
012700*01  AREA -COPY W01160     -PRE I60-                                      
012800     EJECT                                                                
012900 01  W001-AREA-START             PIC X(24)     VALUE                      
013000                                 'W001-AREA-START  '.                     
013100     SKIP2                                                                
013200 01  W001-HJALPAREOR.                                                     
013300*                                                                         
013400     03  W001-SKIP               PIC 9(3)      COMP-3  VALUE 1.           
013500     03  W001-ANTAL-RADER                                                 
013600                                 PIC 9(3)      VALUE 999.                 
013700     03  W001-MAX-RADER-PER-SIDA                                          
013800                                 PIC 9(3)      VALUE 42.                  
013900     03  W001-MAX-POSITIONER-PER-RAD                                      
014000                                 PIC 9(3)      VALUE 120.                 
014100     03  W001-LISTNR             PIC X(11)     VALUE 'W22158-001'.        
014200     03  W001-SIDRAKNARE         PIC S9(5)     COMP-3 VALUE ZERO.         
014300     EJECT                                                                
014400                                                                          
014500 01  W001-RAD.                                                            
014600                                                                          
014700     03  FILLER                  PIC X(121)    VALUE SPACE.               
014800     EJECT                                                                
014900                                                                          
015000 01  W001-RUBRIK1.                                                        
015100                                                                          
015200     03  FILLER                  PIC X(3)      VALUE SPACE.               
015300     03  FILLER                  PIC X(21)                                
015400                                 VALUE 'VOLVO CAR PARTS      '.           
015500                                                                          
015600     03  FILLER                  PIC X(3)      VALUE SPACE.               
015700     03  FILLER                  PIC X(12)                                
015800                                 VALUE 'W22158-001'.                      
015900                                                                          
016000     03  FILLER                  PIC X(10)     VALUE SPACE.               
016100     03  FILLER                  PIC X(16)                                
016200                                 VALUE 'MANUELLT PB-PLAN'.                
016300                                                                          
016400     03  FILLER                  PIC X(38)     VALUE SPACE.               
016500     03  W001-DATUM              PIC XXBXXBXX.                            
016600                                                                          
016700     03  FILLER                  PIC X(3)      VALUE SPACE.               
016800     03  FILLER                  PIC X(4)                                 
016900                                 VALUE 'SID'.                             
017000                                                                          
017100     03  W001-SID                PIC Z(4)9.                               
017200     SKIP2                                                                
017300     EJECT                                                                
017400                                                                          
017500 01  W001-RUBRIK2.                                                        
017600     03  FILLER                  PIC X(3)      VALUE SPACE.               
017700     03  FILLER                  PIC X(5)                                 
017800                                 VALUE 'ANSK'.                            
017900                                                                          
018000     03  FILLER                  PIC X(1)      VALUE SPACE.               
018100     03  FILLER                  PIC X(9)                                 
018200                                 VALUE 'ARTIKELNR'.                       
018300                                                                          
018400     03  FILLER                  PIC X(3)      VALUE SPACE.               
018500     03  FILLER                  PIC X(25)                                
018600                                 VALUE 'BENÄMNING'.                       
018700                                                                          
018800     03  FILLER                  PIC X(3)      VALUE SPACE.               
018900     03  FILLER                  PIC X(10)                                
019000                                 VALUE 'LEVERANTÖR'.                      
019100                                                                          
019200     03  FILLER                  PIC X(3)      VALUE SPACE.               
019300     03  FILLER                  PIC X(8)                                 
019400                                 VALUE 'DAT.MAN.'.                        
019500                                                                          
019600     03  FILLER                  PIC X(6)      VALUE SPACE.               
019700     03  FILLER                  PIC X(11)                                
019800                                 VALUE 'MAN.PB-PLAN'.                     
019900                                                                          
020000     03  FILLER                  PIC X(4)      VALUE SPACE.               
020100     03  FILLER                  PIC X(12)                                
020200                                 VALUE 'MASK.PB-PLAN'.                    
020300     EJECT                                                                
020400 01  W001-DETALJRAD.                                                      
020500                                                                          
020600     03  FILLER                  PIC X(2)      VALUE SPACE.               
020700     03  W001-IDANSK             PIC Z(1)9(3).                            
020800                                                                          
020900     03  FILLER                  PIC X(3)      VALUE SPACE.               
021000     03  W001-IDARTNR            PIC Z(8)9(1).                            
021100                                                                          
021200     03  FILLER                  PIC X(3)      VALUE SPACE.               
021300     03  W001-BEART              PIC X(25).                               
021400                                                                          
021500     03  FILLER                  PIC X(8)      VALUE SPACE.               
021600     03  W001-IDLEVNR            PIC X(5).                                
021700                                                                          
021800     03  FILLER                  PIC X(3)      VALUE SPACE.               
021900     03  W001-DAPBPLAN           PIC Z(7)9(1).                            
022000                                                                          
022100     03  FILLER                  PIC X(7)      VALUE SPACE.               
022200     03  W001-KVPB-PLAN          PIC Z(6).9(1).                           
022300                                                                          
022400     03  FILLER                  PIC X(6)      VALUE SPACE.               
022500     03  W001-MASKPB             PIC Z(6).9(1).                           
022600                                                                          
022700     EJECT                                                                
022800 01  SORTWS-AREA-START           PIC X(24)     VALUE                      
022900                                  'SORTWS-AREA-START  '.                  
023000     SKIP2                                                                
023100                                                                          
023200 01  SORTWS-AREA.                                                         
023300                                                                          
023400     03  SORTWS-CLAG-IDARTNR     PIC S9(9)      COMP-3.                   
023500     03  SORTWS-CLAG-IDANSK      PIC S9(3)      COMP-3.                   
023600     03  SORTWS-CLAG-KVPB-PLAN   PIC S9(6)V9(1) COMP-3.                   
023700     03  SORTWS-CLAG-DAPBPLAN    PIC  9(8).                               
023800     03  SORTWS-CLAG-IDLEVNR     PIC X(5).                                
023900                                                                          
024000     SKIP2                                                                
024100                                                                          
024200 01  SORT-RETURN-X               PIC X(2)      VALUE SPACE.               
024300                                                                          
024400     EJECT                                                                
024500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024600                                                                          
024700     EJECT                                                                
024800 01  FILLER                      PIC X(16)     VALUE 'IMS-WS'.            
024900     SKIP3                                                                
025000                                                                          
025100 01  NYCKLAR-TILL-DLI.                                                    
025200     03  W-IDSKYLT-X.                                                     
025300         05  W-IDSKYLT           PIC X(3)      VALUE 'GB'.                
025400     03  W-IDARTNR-X.                                                     
025500         05  W-IDARTNR           PIC S9(9)     VALUE ZERO COMP-3.         
025600     SKIP2                                                                
025700                                                                          
025800*    --- STATUS-KOD FRÅN IMS                                              
025900                                                                          
026000 01  STATUS-WS                   PIC XX.                                  
026100     88  SEGMENT-FINNS                         VALUE '  '.                
026200     88  SEGMENT-FINNS-REDAN                   VALUE 'II'.                
026300     88  SEGMENT-SAKNAS                        VALUE 'GE'.                
026400     SKIP2                                                                
026500                                                                          
026600 01  GODK-STATUSKODER.                                                    
026700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026800     SKIP3                                                                
026900                                                                          
027000 01  SSA1                        PIC X(64).                               
027100 01  SSA2                        PIC X(64).                               
027200                                                                          
027300     EJECT                                                                
027400*    --- IMS FUNKTIONSKODER                                               
027500*01  -COPY W0003                                                          
027600     EJECT                                                                
027700                                                                          
027800*    ---  DLI INPUT-OUTPUT AREA                                           
027900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
028000 01  DLI-IO-WLBENA11.                                                     
028100*    03  -COPY WDD311  -PRE BENA-                                         
028200     EJECT                                                                
028300                                                                          
028400 LINKAGE SECTION.                                                         
028500                                                                          
028600     EJECT                                                                
028700*01  -COPY W0008  -PRE BENA-                                              
028800     05  FILLER                    PIC X.                                 
028900     EJECT                                                                
029000 01  PBTO-WDK6-PCB                 PIC X.                                 
029100 01  PBTO-WDK7-PCB                 PIC X.                                 
029200 01  PBTO-ARTM-PCB                 PIC X.                                 
029300 01  PBTO-2501-PCB                 PIC X.                                 
029400 01  PBTO-WDB6R-PCB                PIC X.                                 
029500 01  PBTO-WDK7R-PCB                PIC X.                                 
029600 01  PBTO-WDB6-PCB                 PIC X.                                 
029700 01  PBTO-WDD7-PCB                 PIC X.                                 
029800 01  PBTO-WDK7E-PCB                PIC X.                                 
029900 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
030000 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
030100 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
030200 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
030300 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
030400 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
030500 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
030600 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
030700     EJECT                                                                
030800                                                                          
030900 PROCEDURE DIVISION  USING BENA-PCB                                       
031000                           PBTO-WDK6-PCB  PBTO-WDK7-PCB                   
031100                           PBTO-ARTM-PCB  PBTO-2501-PCB                   
031200                           PBTO-WDB6R-PCB PBTO-WDK7R-PCB                  
031300                           PBTO-WDB6-PCB  PBTO-WDD7-PCB                   
031400                           PBTO-WDK7E-PCB                                 
031500                           PBTO-W222-UTIL-WDK6-PCB                        
031600                           PBTO-W222-UTIL-WDK7-PCB                        
031700                           PBTO-W222-UTIL-WDB6-PCB                        
031800                           PBTO-W222-UTUP-WDK7-PCB                        
031900                           PBTO-W222-UTUP-WDB6-PCB                        
032000                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
032100                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
032200                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
032300                           .                                              
032400                                                                          
032500 MAIN SECTION.                                                            
032600     ENTRY 'DLITCBL' USING BENA-PCB                                       
032700                           PBTO-WDK6-PCB  PBTO-WDK7-PCB                   
032800                           PBTO-ARTM-PCB  PBTO-2501-PCB                   
032900                           PBTO-WDB6R-PCB PBTO-WDK7R-PCB                  
033000                           PBTO-WDB6-PCB  PBTO-WDD7-PCB                   
033100                           PBTO-WDK7E-PCB                                 
033200                           PBTO-W222-UTIL-WDK6-PCB                        
033300                           PBTO-W222-UTIL-WDK7-PCB                        
033400                           PBTO-W222-UTIL-WDB6-PCB                        
033500                           PBTO-W222-UTUP-WDK7-PCB                        
033600                           PBTO-W222-UTUP-WDB6-PCB                        
033700                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
033800                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
033900                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
034000                           .                                              
034100                                                                          
034200     PERFORM A-INIT                                                       
034300                                                                          
034400                                                                          
034500     SORT SORTFIL ASCENDING KEY SORT-IDANSK                               
034600                                SORT-IDARTNR                              
034700                  INPUT PROCEDURE B-SORT-INPUT                            
034800                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
034900                                                                          
035000     IF SORT-RETURN NOT = 0                                               
035100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
035200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
035300           DELIMITED BY SIZE                                              
035400           INTO FELTEXT-STR                                               
035500       DISPLAY FELTEXT                                                    
035600       MOVE RKOD-ABEND-UTAN-DUMP  TO RKOD-ABEND                           
035700       PERFORM S99-ABEND                                                  
035800     ELSE                                                                 
035900       PERFORM Z-FINIT                                                    
036000                                                                          
036100       MOVE ZERO TO RETURN-CODE                                           
036200       GOBACK                                                             
036300     END-IF                                                               
036400                                                                          
036500     .                                                                    
036600     EJECT                                                                
036700 A-INIT SECTION.                                                          
036800                                                                          
036900     OPEN INPUT  W01160                                                   
037000                                                                          
037100     OPEN OUTPUT W22158-001                                               
037200                                                                          
037300     ACCEPT DAGENS-DATUM  FROM DATE                                       
037400     MOVE DAGENS-DATUM TO W001-DATUM                                      
037500     INSPECT W001-DATUM REPLACING ALL SPACE BY '-'                        
037600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037700     .                                                                    
037800     EJECT                                                                
037900 B-SORT-INPUT  SECTION.                                                   
038000                                                                          
038100     PERFORM S01-LAES-W01160                                              
038200     PERFORM UNTIL END-OF-W01160                                          
038300       IF I60-CLAG-DAPBPLAN > 0                                           
038400         IF I60-CLAG-KDERS <= +020                                        
038500           PERFORM BA-FLYTTA-TILL-SORT                                    
038600           PERFORM S31-SORT-RELEASE                                       
038700         END-IF                                                           
038800       END-IF                                                             
038900       PERFORM S01-LAES-W01160                                            
039000     END-PERFORM                                                          
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 BA-FLYTTA-TILL-SORT SECTION.                                             
039500                                                                          
039600     MOVE I60-CLAG-IDARTNR     TO SORTWS-CLAG-IDARTNR                     
039700     MOVE I60-CLAG-IDANSK      TO SORTWS-CLAG-IDANSK                      
039800     MOVE I60-CLAG-KVPB-PLAN   TO SORTWS-CLAG-KVPB-PLAN                   
039900     MOVE I60-CLAG-DAPBPLAN    TO SORTWS-CLAG-DAPBPLAN                    
040000     MOVE I60-CLAG-IDLEVNR     TO SORTWS-CLAG-IDLEVNR                     
040100     .                                                                    
040200     EJECT                                                                
040300                                                                          
040400 C-SORT-OUTPUT SECTION.                                                   
040500                                                                          
040600     PERFORM S32-SORT-RETURN                                              
040700     PERFORM UNTIL END-OF-SORTFIL                                         
040800       PERFORM CA-HAMTA-BENAMNING                                         
040900       PERFORM CB-HAMTA-MASK-PBPLAN                                       
041000       PERFORM CC-MOVE-SORTWS-TO-W001                                     
041100       PERFORM S21-SKRIV-W22158-001                                       
041200       PERFORM S32-SORT-RETURN                                            
041300     END-PERFORM                                                          
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 CA-HAMTA-BENAMNING SECTION.                                              
041800                                                                          
041900     MOVE SORTWS-CLAG-IDARTNR         TO W-IDARTNR                        
042000                                                                          
042100     PERFORM IMS-GU-BENA-WDD311                                           
042200                                                                          
042300     IF SEGMENT-FINNS                                                     
042400        MOVE BENA-TEXT-BEART          TO W001-BEART                       
042500     ELSE                                                                 
042600        MOVE SPACE                    TO W001-BEART                       
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000                                                                          
043100 CB-HAMTA-MASK-PBPLAN SECTION.                                            
043200                                                                          
043300     MOVE SORTWS-CLAG-IDARTNR         TO PBTO-IDARTNR                     
043400     CALL W222PBTO  USING PBTO-W222PBTO                                   
043500                          PBTO-WDK6-PCB                                   
043600                          PBTO-WDK7-PCB                                   
043700                          PBTO-ARTM-PCB                                   
043800                          PBTO-2501-PCB                                   
043900                          PBTO-WDB6R-PCB                                  
044000                          PBTO-WDK7R-PCB                                  
044100                          PBTO-WDB6-PCB                                   
044200                          PBTO-WDD7-PCB                                   
044300                          PBTO-WDK7E-PCB                                  
044400                          PBTO-W222-UTIL-WDK6-PCB                         
044500                          PBTO-W222-UTIL-WDK7-PCB                         
044600                          PBTO-W222-UTIL-WDB6-PCB                         
044700                          PBTO-W222-UTUP-WDK7-PCB                         
044800                          PBTO-W222-UTUP-WDB6-PCB                         
044900                          PBTO-W222-UTUP-UTIL-WDK6-PCB                    
045000                          PBTO-W222-UTUP-UTIL-WDK7-PCB                    
045100                          PBTO-W222-UTUP-UTIL-WDB6-PCB                    
045200                                                                          
045300     IF PBTO-KDSVAR = JA                                                  
045400        MOVE PBTO-KVPB-PLAN           TO W001-MASKPB                      
045500     ELSE                                                                 
045600        MOVE ZERO                     TO W001-MASKPB                      
045700     END-IF                                                               
045800     .                                                                    
045900     EJECT                                                                
046000                                                                          
046100 CC-MOVE-SORTWS-TO-W001 SECTION.                                          
046200                                                                          
046300     MOVE SORTWS-CLAG-IDARTNR         TO W001-IDARTNR                     
046400     MOVE SORTWS-CLAG-IDANSK          TO W001-IDANSK                      
046500     MOVE SORTWS-CLAG-KVPB-PLAN       TO W001-KVPB-PLAN                   
046600     MOVE SORTWS-CLAG-DAPBPLAN        TO W001-DAPBPLAN                    
046700     MOVE SORTWS-CLAG-IDLEVNR         TO W001-IDLEVNR                     
046800     .                                                                    
046900     EJECT                                                                
047000                                                                          
047100 Z-FINIT SECTION.                                                         
047200     CLOSE W01160                                                         
047300           W22158-001                                                     
047400     SKIP2                                                                
047500     MOVE 'S' TO POSTSUM-OPKOD                                            
047600     CALL POSTSUM USING POSTSUM-PARM                                      
047700     .                                                                    
047800     EJECT                                                                
047900                                                                          
048000 S01-LAES-W01160  SECTION.                                                
048100     READ W01160 INTO I60-AREA                                            
048200     AT END                                                               
048300        MOVE HIGH-VALUE TO I60-AREA                                       
048400        SET END-OF-W01160 TO TRUE                                         
048500                                                                          
048600     NOT AT END                                                           
048700        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
048800        MOVE 'W22158D1' TO POSTSUM-DDNAMN2                                
048900        MOVE SPACE TO POSTSUM-TRANSTYP                                    
049000        CALL POSTSUM USING POSTSUM-PARM                                   
049100     END-READ                                                             
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500 S21-SKRIV-W22158-001  SECTION.                                           
049600                                                                          
049700     MOVE 1 TO W001-SKIP                                                  
049800     IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                        
049900       PERFORM S21A-SKRIV-RUBRIKER                                        
050000     END-IF                                                               
050100     SKIP2                                                                
050200                                                                          
050300     IF SORTWS-CLAG-IDANSK NOT = WS-SAVE-IDANSK AND                       
050400        WS-FIRST-IDANSK NOT = 'J'                                         
050500        PERFORM S21A-SKRIV-RUBRIKER                                       
050600        WRITE W22158-001-RAD FROM W001-DETALJRAD AFTER W001-SKIP          
050700     ELSE                                                                 
050800        WRITE W22158-001-RAD FROM W001-DETALJRAD AFTER W001-SKIP          
050900     END-IF                                                               
051000                                                                          
051100     SKIP2                                                                
051200     MOVE SORTWS-CLAG-IDANSK TO WS-SAVE-IDANSK                            
051300     MOVE 'N' TO WS-FIRST-IDANSK                                          
051400     MOVE SPACE TO W001-RAD                                               
051500     ADD  +1 TO W001-ANTAL-RADER                                          
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900 S21A-SKRIV-RUBRIKER SECTION.                                             
052000                                                                          
052100     ADD +1 TO W001-SIDRAKNARE                                            
052200     MOVE W001-SIDRAKNARE TO W001-SID                                     
052300     WRITE W22158-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
052400     WRITE W22158-001-RAD FROM W001-RUBRIK2 AFTER 2                       
052500     MOVE +7 TO W001-ANTAL-RADER                                          
052600     SKIP2                                                                
052700     MOVE 3 TO W001-SKIP                                                  
052800     .                                                                    
052900     EJECT                                                                
053000                                                                          
053100 S31-SORT-RELEASE  SECTION.                                               
053200                                                                          
053300     RELEASE SORT-POST FROM SORTWS-AREA                                   
053400     .                                                                    
053500     EJECT                                                                
053600                                                                          
053700 S32-SORT-RETURN  SECTION.                                                
053800                                                                          
053900     RETURN SORTFIL INTO SORTWS-AREA                                      
054000     AT END                                                               
054100         SET END-OF-SORTFIL TO TRUE                                       
054200     .                                                                    
054300     EJECT                                                                
054400                                                                          
054500 S99-ABEND SECTION.                                                       
054600                                                                          
054700     SKIP2                                                                
054800     MOVE 'S' TO POSTSUM-OPKOD                                            
054900     CALL POSTSUM USING POSTSUM-PARM                                      
055000     CALL ABEND USING RKOD-ABEND                                          
055100     .                                                                    
055200     EJECT                                                                
055300                                                                          
055400* --- IMS SEKTIONER ---                                                   
055500     SKIP3                                                                
055600 IMS-GU-BENA-WDD311 SECTION.                                              
055700                                                                          
055800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
055900          DELIMITED BY SIZE INTO SSA1                                     
056000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
056100          DELIMITED BY SIZE INTO SSA2                                     
056200     MOVE '  GE' TO GODK-STATUSKODER                                      
056300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
056400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
056500     PERFORM IMS-STATUSKONTROLL                                           
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 IMS-STATUSKONTROLL SECTION.                                              
057000                                                                          
057100     SET STATUS-IX TO 1                                                   
057200     SEARCH GODK-STATUS                                                   
057300       AT END                                                             
057400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
057500           DELIMITED BY SIZE INTO FELTEXT                                 
057600         DISPLAY FELTEXT                                                  
057700         CALL FELLOG                                                      
057800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
057900         CONTINUE                                                         
058000     END-SEARCH                                                           
058100     .                                                                    
