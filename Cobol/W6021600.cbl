000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6021600.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/02/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER KVALITETS SPÄRRADE ARTIKLAR.                               
000900*        BESTÄLLNING AV LISTA W614S1.                                     
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDR5                                       
001200*        PROGRAMMET LÄSER      WDB6                                       
001300*        PROGRAMMET LÄSER      WDP3                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W6T216                                              
001700*        MID:         W6I21601                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W6O21601                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6021600'.            
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
003600 77  WS-KDLEVSP                  PIC 9(2)    VALUE ZERO.                  
003700 77  WS-FLKVANT                  PIC X       VALUE SPACE.                 
003800 77  WS-KVSPARR-KVAL             PIC 9(7)    VALUE ZERO.                  
003900 77  WS-KDSORT1                  PIC 9       VALUE ZERO.                  
004000 77  WS-IDFKNGRP                 PIC X(4)    VALUE ZERO.                  
004100 77  IDFKNGRP-SOEK               PIC X       VALUE 'N'.                   
004200 77  KDLEVSP-SOEK                PIC X       VALUE 'N'.                   
004300 77  IDPERSON-SOEK               PIC X       VALUE 'N'.                   
004400 77  KVANT-SOEK                  PIC X       VALUE 'N'.                   
004500 77  IDUSER-SOEK                 PIC X       VALUE 'N'.                   
004600 77  IDDC-SW                     PIC X       VALUE 'J'.                   
004700 77  INDX                        PIC S9(3)   VALUE ZERO.                  
004800 77  MAX-INDX                    PIC S9(3)   VALUE +10.                   
004900                                                                          
005000 01  WS-FKNURVAL                 PIC 9(4)    VALUE ZERO.                  
005100 01  FILLER REDEFINES WS-FKNURVAL.                                        
005200     03 WS-GRP-00                PIC 9(2).                                
005300     03 WS-RESTEN-00             PIC 9(2).                                
005400 01  FILLER REDEFINES WS-FKNURVAL.                                        
005500     03 WS-GRP-000               PIC 9(1).                                
005600     03 WS-RESTEN-000            PIC 9(3).                                
005700                                                                          
005800 01  WS-DCURVAL                  PIC X(2)    VALUE SPACE.                 
005900 01  FILLER REDEFINES WS-DCURVAL.                                         
006000     03 WS-FORSTA-DC             PIC X.                                   
006100     03 WS-SISTA-DC              PIC X.                                   
006200                                                                          
006300 77  IDPERSON-SW                 PIC X       VALUE 'N'.                   
006400     88  IDPERSON-OK                         VALUE 'J'.                   
006500     88  IDPERSON-NOT-OK                     VALUE 'N'.                   
006600                                                                          
006700 77  EOF-WDR5-SW                 PIC X       VALUE 'N'.                   
006800     88  EOF-WDR5-YES                        VALUE 'J'.                   
006900     88  EOF-WDR5-NO                         VALUE 'N'.                   
007000                                                                          
007100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007200     88  NYCKLAR-OK                          VALUE 'J'.                   
007300     88  NYCKLAR-FEL                         VALUE 'N'.                   
007400                                                                          
007500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007600     88  INDATA-OK                           VALUE 'J'.                   
007700     88  INDATA-FEL                          VALUE 'N'.                   
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '6216'.                
008100     88  GODK-MID                            VALUE '6216'.                
008200     EJECT                                                                
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008700     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009200*01 -COPY WMEDAREA                                                        
009300     SKIP3                                                                
009400 01  MESSAGE-CODES.                                                       
009500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009800     03  PRESS-PF4-FOR-PRINT-OUT PIC X(3)    VALUE '081'.                 
009900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010000     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
010100     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
010200     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
010300 01  MESSAGE-TEXT1.                                                       
010400     03  PRINTING-NOT-ALLOWED-WRONG-DC                                    
010500                                 PIC X(31)   VALUE                        
010600         'PRINTING NOT ALLOWED - WRONG DC'.                               
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011000     SKIP3                                                                
011100*01 -COPY W006PRT                                                         
011200     EJECT                                                                
011300*01 -COPY WMSGINIT                                                        
011400     EJECT                                                                
011500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
011600 01  SPAR-AREA.                                                           
011700     03  SPAR-IDTRANS                PIC X(4) VALUE '6216'.               
011800     03  SPAR-IDDC-ENTER             PIC X(2).                            
011900     03  SPAR-IDDC-NEXT              PIC X(2).                            
012000     03  SPAR-IDARTNR-ENTER          PIC 9(9).                            
012100     03  SPAR-IDARTNR-NEXT           PIC 9(9).                            
012200     03  SPAR-IDFKNGRP-ENTER         PIC 9(4).                            
012300     03  SPAR-IDFKNGRP-NEXT          PIC 9(4).                            
012400     03  SPAR-KDLEVSP-ENTER          PIC 9(2).                            
012500     03  SPAR-KDLEVSP-NEXT           PIC 9(2).                            
012600     03  SPAR-KVSPARR-ENTER          PIC 9(7).                            
012700     03  SPAR-KVSPARR-NEXT           PIC 9(7).                            
012800     03  SPAR-IDUSER-ENTER           PIC X(8).                            
012900     03  SPAR-IDUSER-NEXT            PIC X(8).                            
013000     EJECT                                                                
013100 01  PROG-TO-PROG-SW.                                                     
013200*    03  -COPY WMSGSOP                                                    
013300     EJECT                                                                
013400 01  WS-PARAMETRAR.                                                       
013500     03  WS-URVAL.                                                        
013600         05 URV-KDSORT1     PIC 9.                                        
013700         05 URV-IDDC-BEST   PIC X(2).                                     
013800         05 URV-KDLEVSP     PIC 9(2).                                     
013900         05 URV-IDFKNGRP    PIC 9(4).                                     
014000         05 URV-FLKVANT     PIC X.                                        
014100         05 URV-IDDC        PIC X(2).                                     
014200         05 URV-IDUSER      PIC X(8).                                     
014300         05 URV-IDPERSON    PIC X(3).                                     
014400     03  WS-PRINTER.                                                      
014500         05 URV-IDPRINTER   PIC X(8) VALUE SPACE.                         
014600     EJECT                                                                
014700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014900     SKIP3                                                                
015000*01  MID -COPY W6I21601                                                   
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015300     SKIP3                                                                
015400*01  -COPY WMSGAREA                                                       
015500     EJECT                                                                
015600     03  MOD REDEFINES MSG-AREA.                                          
015700*      05  -COPY W6O21601                                                 
015800     EJECT                                                                
015900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016000     SKIP3                                                                
016100*01  -COPY WMFSAREA                                                       
016200     EJECT                                                                
016300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016500     SKIP3                                                                
016600 01  NYCKLAR-TILL-DLI.                                                    
016700                                                                          
016800     03  W-IDDC-MIN-NYCKEL-X.                                             
016900         05  W-IDDC-MIN-NYCKEL   PIC X(2)    VALUE SPACE.                 
017000     03  W-IDDC-MAX-NYCKEL-X.                                             
017100         05  W-IDDC-MAX-NYCKEL   PIC X(2)    VALUE SPACE.                 
017200     03  W-IDDC-MIN-X.                                                    
017300         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
017400     03  W-IDDC-MAX-X.                                                    
017500         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
017600     03  W-IDFKNGRP-MIN-X.                                                
017700         05  W-IDFKNGRP-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
017800     03  W-IDFKNGRP-MAX-X.                                                
017900         05  W-IDFKNGRP-MAX      PIC S9(5)   VALUE +99999 COMP-3.         
018000     03  W-KDLEVSP-MIN-X.                                                 
018100         05  W-KDLEVSP-MIN       PIC S9(3)   VALUE ZERO COMP-3.           
018200     03  W-KDLEVSP-MAX-X.                                                 
018300         05  W-KDLEVSP-MAX       PIC S9(3)   VALUE +999 COMP-3.           
018400     03  W-KVSPARR-MIN-X.                                                 
018500         05  W-KVSPARR-MIN       PIC S9(7)   VALUE ZERO COMP-3.           
018600     03  W-KVSPARR-MAX-X.                                                 
018700         05  W-KVSPARR-MAX       PIC S9(7)  VALUE +9999999 COMP-3.        
018800     03  W-IDUSER-MIN-X.                                                  
018900         05  W-IDUSER-MIN        PIC X(8)    VALUE LOW-VALUE.             
019000     03  W-IDUSER-MAX-X.                                                  
019100         05  W-IDUSER-MAX        PIC X(8)    VALUE HIGH-VALUE.            
019200                                                                          
019300     03  W-2403KEY-X.                                                     
019400         05  W-2403-IDHTYP      PIC X(4)     VALUE '2403'.                
019500         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
019600     03  W-2404KEY-MIN-X.                                                 
019700         05  W-IDARTNR-MIN      PIC S9(9)    VALUE ZERO COMP-3.           
019800         05  W-IDDC-KY-MIN      PIC X(2)     VALUE LOW-VALUE.             
019900         05  W-IDFKNGRP-KY-MIN  PIC S9(5)    VALUE ZERO COMP-3.           
020000     03  W-2404KEY-MAX-X.                                                 
020100         05  W-IDARTNR-MAX      PIC S9(9)  VALUE 999999999 COMP-3.        
020200         05  W-IDDC-KY-MAX      PIC X(2)     VALUE HIGH-VALUE.            
020300         05  W-IDFKNGRP-KY-MAX  PIC S9(5)    VALUE +99999 COMP-3.         
020400                                                                          
020500     03  W-IDDC-B6-X.                                                     
020600         05 W-IDDC-B6            PIC X(2).                                
020700                                                                          
020800     03  W-KDARBTYP-X.                                                    
020900         05  W-KDARBTYP          PIC X(8)    VALUE 'QUAL    '.            
021000     03  W-IDPERSON-X.                                                    
021100         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
021200     03  W-WDP321KY-MIN-X.                                                
021300         05  W-IDLANDX2-P321-MIN PIC X(2)    VALUE 'SE'.                  
021400         05  W-IDARTNR-FOM-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
021500         05  W-IDARTNR-TOM-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
021600     03  W-WDP321KY-MAX-X.                                                
021700         05  W-IDLANDX2-P321-MAX PIC X(2)    VALUE 'SE'.                  
021800         05  W-IDARTNR-FOM-MAX   PIC S9(9) VALUE 999999999 COMP-3.        
021900         05  W-IDARTNR-TOM-MAX   PIC S9(9) VALUE 999999999 COMP-3.        
022000     03  W-WDP322KY-X.                                                    
022100         05  W-IDLANDX2-P322     PIC X(2)    VALUE 'SE'.                  
022200         05  W-IDLEVNR           PIC X(5)    VALUE LOW-VALUE.             
022300     03  W-WDP323KY-MIN-X.                                                
022400         05  W-IDLANDX2-P321-MIN PIC X(2)    VALUE 'SE'.                  
022500         05  W-IDFKNGRP-FOM-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
022600         05  W-IDFKNGRP-TOM-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
022700     03  W-WDP323KY-MAX-X.                                                
022800         05  W-IDLANDX2-P321-MAX PIC X(2)    VALUE 'SE'.                  
022900         05  W-IDFKNGRP-FOM-MAX  PIC S9(5)   VALUE 99999 COMP-3.          
023000         05  W-IDFKNGRP-TOM-MAX  PIC S9(5)   VALUE 99999 COMP-3.          
023100     03  W-IDARTNR-X.                                                     
023200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023300     03  W-IDFKNGRP-X.                                                    
023400         05  W-IDFKNGRP          PIC S9(5)   VALUE ZERO COMP-3.           
023500                                                                          
023600     EJECT                                                                
023700*    --- STATUS-KOD FRÅN IMS                                              
023800 01  STATUS-WS                   PIC XX.                                  
023900     88  SEGMENT-FINNS                       VALUE '  '.                  
024000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024200     88  BASEN-SLUT                          VALUE 'GB'.                  
024300     SKIP2                                                                
024400 01  GODK-STATUSKODER.                                                    
024500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024600     SKIP3                                                                
024700 01  SSA1                        PIC X(224).                              
024800 01  SSA2                        PIC X(224).                              
024900     EJECT                                                                
025000*    --- IMS FUNKTIONSKODER                                               
025100*01  -COPY W0003                                                          
025200     EJECT                                                                
025300*    ---  DLI INPUT-OUTPUT AREA                                           
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2404'.                    
025500 01  DLI-IO-WDGX2404.                                                     
025600*    03  -COPY WDGX2404                                                   
025700                                                                          
025800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
025900 01   DLI-IO-AREA-B601.                                                   
026000*     03  -COPY WDB601                                                    
026100                                                                          
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
026300 01  DLI-IO-WDP301.                                                       
026400*    03  -COPY WDP301                                                     
026500     EJECT                                                                
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
026700 01  DLI-IO-WDP311.                                                       
026800*    03  -COPY WDP311                                                     
026900     EJECT                                                                
027000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP321'.                      
027100 01  DLI-IO-WDP321.                                                       
027200*    03  -COPY WDP321                                                     
027300     EJECT                                                                
027400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP322'.                      
027500 01  DLI-IO-WDP322.                                                       
027600*    03  -COPY WDP322                                                     
027700     EJECT                                                                
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP323'.                      
027900 01  DLI-IO-WDP323.                                                       
028000*    03  -COPY WDP323                                                     
028100     EJECT                                                                
028200     EJECT                                                                
028300 LINKAGE SECTION.                                                         
028400*01  -COPY W0009   -PRE MSG-                                              
028500     EJECT                                                                
028600*01  -COPY W0009   -PRE ALT-                                              
028700     EJECT                                                                
028800*01  -COPY W0008   -PRE USEA-                                             
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01  -COPY W0008   -PRE 2404-                                             
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008   -PRE WDB6-                                             
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008   -PRE WDP3-                                             
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 2404-PCB              
030100                           WDB6-PCB WDP3-PCB.                             
030200 MAIN SECTION.                                                            
030300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 2404-PCB              
030400                           WDB6-PCB WDP3-PCB.                             
030500                                                                          
030600     PERFORM IMS-GET-MSG                                                  
030700     IF SEGMENT-FINNS                                                     
030800        PERFORM A-INIT                                                    
030900        PERFORM B-KOLLA-NYCKLAR                                           
031000        IF NYCKLAR-OK                                                     
031100           IF MFS-FIRST                                                   
031200              PERFORM C-FOERSTA-SIDA                                      
031300           ELSE                                                           
031400              IF MFS-NEXT                                                 
031500                 PERFORM D-NAESTA-SIDA                                    
031600              ELSE                                                        
031700                 IF MFS-PRINT                                             
031800                    PERFORM G-KOLLA-INDATA                                
031900                    IF INDATA-OK                                          
032000                       PERFORM H-STARTA-SOP                               
032100                    END-IF                                                
032200                 ELSE                                                     
032300                    PERFORM E-SAMMA-SIDA                                  
032400                 END-IF                                                   
032500              END-IF                                                      
032600           END-IF                                                         
032700           PERFORM F-LAES-VISA-INFO                                       
032800        END-IF                                                            
032900     END-IF                                                               
033000     COMPUTE MSG-KVLL = LENGTH OF MOD-W6O21601 + 4                        
033100     PERFORM IMS-INSERT-MSG                                               
033200                                                                          
033300     MOVE ZERO TO RETURN-CODE                                             
033400     GOBACK                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 A-INIT SECTION.                                                          
033800                                                                          
033900     IF MSG-DUBBLA-TRANSKODER                                             
034000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I21601                 
034100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
034200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
034300     ELSE                                                                 
034400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I21601                  
034500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
034600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034700     END-IF                                                               
034800                                                                          
034900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
035000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
035100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
035200                                                                          
035300     MOVE LOW-VALUE TO MSG-AREA                                           
035400     MOVE 'W6O216N1' TO MFS-IDMOD                                         
035500     MOVE '6216' TO MOD-IDTRANS                                           
035600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
035700                                                                          
035800     IF EGEN-MID                                                          
035900       CONTINUE                                                           
036000     ELSE                                                                 
036100       MOVE ALL '+' TO MID-IDDC-IN                                        
036200                       MID-IDFKNGRP-IN                                    
036300                       MID-FL-KVSPARR-KVAL-IN                             
036400                       MID-IDUSER-SPKVAL-IN                               
036500                       MID-KDLEVSP-IN                                     
036600                       MID-IDPERSON-IN                                    
036700       MOVE ZERO    TO MID-KDLEVSP-UT                                     
036800       MOVE SPACE   TO MID-FL-KVSPARR-KVAL-UT                             
036900                       MID-IDUSER-SPKVAL-UT                               
037000       MOVE SPACE TO MFS-KDTRTYP                                          
037100       MOVE '7' TO MFS-IDPFK                                              
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 B-KOLLA-NYCKLAR SECTION.                                                 
037600                                                                          
037700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037800     MOVE '001'             TO MSGI-KDCALL                                
037900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
038000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
038100     MOVE '6216'            TO MSGI-IDTRANS                               
038200                                                                          
038300     IF MID-IDFKNGRP-IN NOT = ALL '+'                                     
038400        INSPECT MID-IDFKNGRP-IN REPLACING                                 
038500                         LEADING SPACE BY ZERO                            
038600        MOVE MID-IDFKNGRP-IN TO MSGI-IDFKNGRP                             
038700     END-IF                                                               
038800                                                                          
038900     IF MID-IDDC-IN NOT = ALL '+'                                         
039000        MOVE MID-IDDC-IN TO MSGI-IDDC-KEY                                 
039100     END-IF                                                               
039200                                                                          
039300     IF MID-IDPERSON-IN NOT = ALL '+'                                     
039400        MOVE MID-IDPERSON-IN TO MSGI-IDPERSON                             
039500     END-IF                                                               
039600     INSPECT MSGI-IDPERSON REPLACING                                      
039700                         LEADING SPACE BY ZERO                            
039800                                                                          
039900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
040100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
040200                                                                          
040300     IF MID-KDLEVSP-IN NOT = ALL '+'                                      
040400        INSPECT MID-KDLEVSP-IN REPLACING LEADING SPACE BY ZERO            
040500        MOVE MID-KDLEVSP-IN TO WS-KDLEVSP                                 
040600     ELSE                                                                 
040700        INSPECT MID-KDLEVSP-UT REPLACING LEADING SPACE BY ZERO            
040800        MOVE MID-KDLEVSP-UT TO WS-KDLEVSP                                 
040900     END-IF                                                               
041000                                                                          
041100     IF MID-FL-KVSPARR-KVAL-IN NOT = ALL '+'                              
041200        MOVE MID-FL-KVSPARR-KVAL-IN TO WS-FLKVANT                         
041300     ELSE                                                                 
041400        MOVE MID-FL-KVSPARR-KVAL-UT TO WS-FLKVANT                         
041500     END-IF                                                               
041600                                                                          
041700     IF MID-IDUSER-SPKVAL-IN NOT = ALL '+'                                
041800        MOVE MID-IDUSER-SPKVAL-IN TO WS-IDUSER                            
041900     ELSE                                                                 
042000        MOVE MID-IDUSER-SPKVAL-UT TO WS-IDUSER                            
042100     END-IF                                                               
042200                                                                          
042300     MOVE JA TO NYCKLAR-SW                                                
042400     MOVE NEJ TO IDFKNGRP-SOEK                                            
042500                 KDLEVSP-SOEK                                             
042600                 KVANT-SOEK                                               
042700                 IDUSER-SOEK                                              
042800                 IDPERSON-SOEK                                            
042900                                                                          
043000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
043100     IF MID-IDDC-IN NOT = ALL '+'                                         
043200       MOVE '7'         TO MFS-IDPFK                                      
043300       MOVE SPACE       TO MFS-KDTRTYP                                    
043400       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
043500     END-IF                                                               
043600                                                                          
043700     MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-IN                              
043800     IF MID-IDFKNGRP-IN NOT = ALL '+'                                     
043900       MOVE '7'           TO MFS-IDPFK                                    
044000       MOVE SPACE         TO MFS-KDTRTYP                                  
044100       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
044200     END-IF                                                               
044300                                                                          
044400     MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-IN                               
044500     IF MID-KDLEVSP-IN NOT = ALL '+'                                      
044600       MOVE '7'         TO MFS-IDPFK                                      
044700       MOVE SPACE       TO MFS-KDTRTYP                                    
044800       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
044900     END-IF                                                               
045000                                                                          
045100     MOVE MFS-RENSA-FAELT TO MOD-FL-KVSPARR-KVAL-IN                       
045200     IF MID-FL-KVSPARR-KVAL-IN NOT = ALL '+'                              
045300       MOVE '7'         TO MFS-IDPFK                                      
045400       MOVE SPACE       TO MFS-KDTRTYP                                    
045500       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
045600     END-IF                                                               
045700                                                                          
045800     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-SPKVAL-IN                         
045900     IF MID-IDUSER-SPKVAL-IN NOT = ALL '+'                                
046000       MOVE '7'         TO MFS-IDPFK                                      
046100       MOVE SPACE       TO MFS-KDTRTYP                                    
046200       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
046300     END-IF                                                               
046400                                                                          
046500     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-IN                              
046600     IF MID-IDPERSON-IN NOT = ALL '+'                                     
046700       MOVE '7'         TO MFS-IDPFK                                      
046800       MOVE SPACE       TO MFS-KDTRTYP                                    
046900       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
047000     END-IF                                                               
047100                                                                          
047200     MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                      
047300     PERFORM IMS-GU-WDB601                                                
047400     IF (DCS-KDDC > SPACE AND NOT DCS-DDC)                                
047500     OR W-IDDC-B6 = ZERO                                                  
047600     OR W-IDDC-B6 = SPACE                                                 
047700     OR W-IDDC-B6 NUMERIC                                                 
047800*    OR W-IDDC-B6 = '1 '                                                  
047900*       IF W-IDDC-B6 = '20' OR '40' OR '60' OR '1 '                       
048000        IF W-IDDC-B6 = '20' OR '40' OR '60'                               
048100        OR ZERO OR SPACE                                                  
048200           IF W-IDDC-B6 = ZERO OR SPACE                                   
048300              MOVE LOW-VALUE  TO W-IDDC-MIN                               
048400                                 W-IDDC-MIN-NYCKEL                        
048500              MOVE HIGH-VALUE TO W-IDDC-MAX                               
048600                                 W-IDDC-MAX-NYCKEL                        
048700           ELSE                                                           
048800              MOVE W-IDDC-B6 TO WS-DCURVAL                                
048900              IF W-IDDC-B6 = '20' OR '60'                                 
049000                 MOVE W-IDDC-B6  TO WS-DCURVAL                            
049100                 MOVE '9'        TO WS-SISTA-DC                           
049200                 MOVE W-IDDC-B6  TO W-IDDC-MIN                            
049300                                    W-IDDC-MIN-NYCKEL                     
049400                 MOVE WS-DCURVAL TO W-IDDC-MAX                            
049500                                    W-IDDC-MAX-NYCKEL                     
049600              ELSE                                                        
049700                 IF W-IDDC-B6 = '40'                                      
049800                    MOVE W-IDDC-B6 TO W-IDDC-MIN                          
049900                                      W-IDDC-MIN-NYCKEL                   
050000                    MOVE '43'      TO W-IDDC-MAX                          
050100                                      W-IDDC-MAX-NYCKEL                   
050200*                                                                         
050300*                ELSE                                                     
050400*                                                                         
050500*                   IF W-IDDC-B6 = '1 '                                   
050600*                      MOVE W-IDDC-B6  TO WS-DCURVAL                      
050700*                      MOVE 'Z'        TO WS-SISTA-DC                     
050800*                      MOVE W-IDDC-B6  TO W-IDDC-MIN                      
050900*                                         W-IDDC-MIN-NYCKEL               
051000*                      MOVE WS-DCURVAL TO W-IDDC-MAX                      
051100*                                         W-IDDC-MAX-NYCKEL               
051200*                   END-IF                                                
051300                END-IF                                                    
051400              END-IF                                                      
051500           END-IF                                                         
051600        ELSE                                                              
051700           MOVE W-IDDC-B6 TO W-IDDC-MIN                                   
051800                             W-IDDC-MIN-NYCKEL                            
051900                             W-IDDC-MAX                                   
052000                             W-IDDC-MAX-NYCKEL                            
052100        END-IF                                                            
052200     ELSE                                                                 
052300        MOVE NEJ TO NYCKLAR-SW                                            
052400     END-IF                                                               
052500                                                                          
052600***  IF MSGI-IDFKNGRP NUMERIC                                             
052700***     IF MSGI-IDFKNGRP > ZERO                                           
052800***        MOVE MSGI-IDFKNGRP TO WS-FKNURVAL                              
052900***        IF WS-RESTEN > ZERO                                            
053000***           MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                          
053100***                               W-IDFKNGRP-MAX                          
053200***        ELSE                                                           
053300***           MOVE ZERO TO WS-RESTEN                                      
053400***           MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                          
053500***           MOVE 99 TO WS-RESTEN                                        
053600***           MOVE WS-FKNURVAL TO W-IDFKNGRP-MAX                          
053700***        END-IF                                                         
053800***        MOVE JA TO IDFKNGRP-SOEK                                       
053900***     END-IF                                                            
054000***  ELSE                                                                 
054100***     MOVE NEJ TO NYCKLAR-SW                                            
054200***  END-IF                                                               
054300                                                                          
054400     IF MSGI-IDFKNGRP NUMERIC                                             
054500        IF MSGI-IDFKNGRP > ZERO                                           
054600           MOVE MSGI-IDFKNGRP TO WS-FKNURVAL                              
054700           IF WS-RESTEN-00 > ZERO                                         
054800              MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                          
054900                                  W-IDFKNGRP-MAX                          
055000           ELSE                                                           
055100              IF WS-RESTEN-000 = ZERO                                     
055200                 MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                       
055300                 MOVE 999 TO WS-RESTEN-000                                
055400                 MOVE WS-FKNURVAL TO W-IDFKNGRP-MAX                       
055500              ELSE                                                        
055600                 IF WS-RESTEN-00 = ZERO                                   
055700                    MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                    
055800                    MOVE 99 TO WS-RESTEN-00                               
055900                    MOVE WS-FKNURVAL TO W-IDFKNGRP-MAX                    
056000                 END-IF                                                   
056100              END-IF                                                      
056200           END-IF                                                         
056300           MOVE JA TO IDFKNGRP-SOEK                                       
056400        END-IF                                                            
056500     ELSE                                                                 
056600        MOVE NEJ TO NYCKLAR-SW                                            
056700     END-IF                                                               
056800                                                                          
056900     IF WS-KDLEVSP NUMERIC                                                
057000        IF WS-KDLEVSP = ZERO                                              
057100           CONTINUE                                                       
057200        ELSE                                                              
057300           IF WS-KDLEVSP = 20 OR 21 OR 22                                 
057400              MOVE WS-KDLEVSP TO W-KDLEVSP-MIN                            
057500                                 W-KDLEVSP-MAX                            
057600              MOVE JA TO KDLEVSP-SOEK                                     
057700           ELSE                                                           
057800              MOVE NEJ TO NYCKLAR-SW                                      
057900           END-IF                                                         
058000        END-IF                                                            
058100     ELSE                                                                 
058200        MOVE NEJ TO NYCKLAR-SW                                            
058300     END-IF                                                               
058400                                                                          
058500     IF WS-FLKVANT = JA OR NEJ OR SPACE OR 'Y'                            
058600        IF WS-FLKVANT = JA OR 'Y'                                         
058700           MOVE +0000001 TO W-KVSPARR-MIN                                 
058800        ELSE                                                              
058900           IF WS-FLKVANT = NEJ                                            
059000              MOVE +0000000 TO W-KVSPARR-MAX                              
059100           END-IF                                                         
059200        END-IF                                                            
059300        IF WS-FLKVANT = JA OR NEJ OR 'Y'                                  
059400           MOVE JA TO KVANT-SOEK                                          
059500        END-IF                                                            
059600     ELSE                                                                 
059700        MOVE NEJ TO NYCKLAR-SW                                            
059800     END-IF                                                               
059900                                                                          
060000     IF WS-IDUSER NOT = SPACE                                             
060100        MOVE WS-IDUSER TO W-IDUSER-MIN                                    
060200                          W-IDUSER-MAX                                    
060300        MOVE JA TO IDUSER-SOEK                                            
060400     END-IF                                                               
060500                                                                          
060600     IF MSGI-IDPERSON NUMERIC                                             
060700        IF MSGI-IDPERSON = ZERO                                           
060800           CONTINUE                                                       
060900        ELSE                                                              
061000           MOVE MSGI-IDPERSON TO W-IDPERSON                               
061100           MOVE JA TO IDPERSON-SOEK                                       
061200        END-IF                                                            
061300     ELSE                                                                 
061400        MOVE NEJ TO NYCKLAR-SW                                            
061500     END-IF                                                               
061600                                                                          
061700     IF EGEN-MID OR NYCKLAR-OK                                            
061800        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
061900                                MOD-IDFKNGRP-UT                           
062000                                MOD-KDLEVSP-UT                            
062100                                MOD-FL-KVSPARR-KVAL-UT                    
062200                                MOD-IDUSER-SPKVAL-UT                      
062300                                MOD-IDPERSON-UT                           
062400        MOVE MSGI-IDDC-KEY TO MOD-IDDC-UT                                 
062500        INSPECT MOD-IDDC-UT                                               
062600                        REPLACING LEADING ZERO BY SPACE                   
062700        MOVE MSGI-IDFKNGRP TO MOD-IDFKNGRP-UT                             
062800        INSPECT MOD-IDFKNGRP-UT                                           
062900                        REPLACING LEADING ZERO BY SPACE                   
063000        MOVE WS-KDLEVSP  TO MOD-KDLEVSP-UT                                
063100        INSPECT MOD-KDLEVSP-UT                                            
063200                        REPLACING LEADING ZERO BY SPACE                   
063300        MOVE WS-FLKVANT TO MOD-FL-KVSPARR-KVAL-UT                         
063400        MOVE WS-IDUSER TO MOD-IDUSER-SPKVAL-UT                            
063500        MOVE MSGI-IDPERSON TO MOD-IDPERSON-UT                             
063600     ELSE                                                                 
063700        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
063800                                MOD-IDFKNGRP-UT                           
063900                                MOD-KDLEVSP-UT                            
064000                                MOD-FL-KVSPARR-KVAL-UT                    
064100                                MOD-IDUSER-SPKVAL-UT                      
064200                                MOD-IDPERSON-UT                           
064300     END-IF                                                               
064400                                                                          
064500                                                                          
064600     IF NYCKLAR-FEL                                                       
064700        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
064800        CALL WMEDKONV USING MED-WMEDAREA                                  
064900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
065000        PERFORM MFS-RENSA-FAELT-IN                                        
065100        PERFORM MFS-RENSA-FAELT-UT                                        
065200        PERFORM MFS-RENSA-SPAR-NYCKLAR                                    
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 C-FOERSTA-SIDA SECTION.                                                  
065700                                                                          
065800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
065900     CALL WMEDKONV USING MED-WMEDAREA                                     
066000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
066100     PERFORM MFS-RENSA-FAELT-IN                                           
066200     .                                                                    
066300     EJECT                                                                
066400 D-NAESTA-SIDA SECTION.                                                   
066500                                                                          
066600     IF SPAR-IDTRANS = '6216'                                             
066700        MOVE SPAR-IDDC-NEXT     TO W-IDDC-MIN-NYCKEL                      
066800        MOVE SPAR-IDARTNR-NEXT  TO W-IDARTNR-MIN                          
066900        IF SPAR-IDARTNR-NEXT = ZERO                                       
067000           MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR-MIN                       
067100           MOVE SPAR-IDDC-ENTER    TO W-IDDC-MIN-NYCKEL                   
067200           MOVE INF-SISTA-SIDAN TO MED-IDMFSFEL                           
067300           CALL WMEDKONV USING MED-WMEDAREA                               
067400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
067500        END-IF                                                            
067600     ELSE                                                                 
067700        PERFORM MFS-RENSA-FAELT-IN                                        
067800     END-IF                                                               
067900     .                                                                    
068000     EJECT                                                                
068100 E-SAMMA-SIDA SECTION.                                                    
068200                                                                          
068300     IF EGEN-MID                                                          
068400        IF SPAR-IDTRANS = '6216'                                          
068500           MOVE SPAR-IDDC-ENTER     TO W-IDDC-MIN-NYCKEL                  
068600           MOVE SPAR-IDARTNR-ENTER  TO W-IDARTNR-MIN                      
068700           IF MID-KDSORT1 = ALL '+'                                       
068800           AND MID-IDNODE = ALL '+'                                       
068900              PERFORM MFS-RENSA-FAELT-IN                                  
069000           ELSE                                                           
069100              MOVE PRESS-PF4-FOR-PRINT-OUT TO MED-IDMFSINF                
069200              CALL WMEDKONV USING MED-WMEDAREA                            
069300              MOVE MED-MFSINF TO MOD-TEMFSFEL                             
069400              PERFORM EA-MID-INDATA-TILL-MOD                              
069500           END-IF                                                         
069600        ELSE                                                              
069700           PERFORM MFS-RENSA-FAELT-IN                                     
069800        END-IF                                                            
069900     ELSE                                                                 
070000        PERFORM MFS-RENSA-FAELT-IN                                        
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 EA-MID-INDATA-TILL-MOD SECTION.                                          
070500                                                                          
070600     IF MID-KDSORT1 NOT = ALL '+'                                         
070700        MOVE MID-KDSORT1 TO MOD-KDSORT1                                   
070800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSORT1-ATTR                    
070900     ELSE                                                                 
071000        MOVE MFS-RENSA-FAELT TO MOD-KDSORT1                               
071100     END-IF                                                               
071200                                                                          
071300     IF MID-IDNODE NOT = ALL '+'                                          
071400        MOVE MID-IDNODE TO MOD-IDNODE                                     
071500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDNODE-ATTR                     
071600     ELSE                                                                 
071700        MOVE MFS-RENSA-FAELT TO MOD-IDNODE                                
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 F-LAES-VISA-INFO SECTION.                                                
072200                                                                          
072300     PERFORM IMS-GET-WDR501                                               
072400                                                                          
072500     IF SEGMENT-SAKNAS                                                    
072600        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
072700        CALL WMEDKONV USING MED-WMEDAREA                                  
072800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
072900        PERFORM MFS-RENSA-FAELT-UT                                        
073000     ELSE                                                                 
073100        MOVE +1 TO INDX                                                   
073200        PERFORM FA-LAS-FOERSTA                                            
073300        IF SEGMENT-FINNS                                                  
073400           PERFORM FC-READ-WDP3                                           
073500           PERFORM                                                        
073600             UNTIL IDPERSON-OK OR EOF-WDR5-YES                            
073700             PERFORM FB-LAS-NAESTA                                        
073800             IF SEGMENT-FINNS                                             
073900               PERFORM FC-READ-WDP3                                       
074000             ELSE                                                         
074100               SET EOF-WDR5-YES    TO TRUE                                
074200             END-IF                                                       
074300           END-PERFORM                                                    
074400        ELSE                                                              
074500           SET EOF-WDR5-YES        TO TRUE                                
074600        END-IF                                                            
074700        IF IDPERSON-OK                                                    
074800           MOVE 2404-IDARTNR       TO SPAR-IDARTNR-ENTER                  
074900           MOVE 2404-IDDC          TO SPAR-IDDC-ENTER                     
075000           MOVE 2404-IDFKNGRP      TO SPAR-IDFKNGRP-ENTER                 
075100           MOVE 2404-KDLEVSP       TO SPAR-KDLEVSP-ENTER                  
075200           MOVE 2404-KVSPARR-KVAL  TO SPAR-KVSPARR-ENTER                  
075300           MOVE 2404-IDUSER-SPKVAL TO SPAR-IDUSER-ENTER                   
075400        ELSE                                                              
075500           MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                          
075600           CALL WMEDKONV USING MED-WMEDAREA                               
075700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
075800           PERFORM MFS-RENSA-FAELT-UT                                     
075900           MOVE W-IDARTNR-MIN      TO SPAR-IDARTNR-ENTER                  
076000           MOVE W-IDDC-MIN         TO SPAR-IDDC-ENTER                     
076100           MOVE W-IDFKNGRP-MIN     TO SPAR-IDFKNGRP-ENTER                 
076200           MOVE W-KDLEVSP-MIN      TO SPAR-KDLEVSP-ENTER                  
076300           MOVE W-KVSPARR-MIN      TO SPAR-KVSPARR-ENTER                  
076400           MOVE W-IDUSER-MIN       TO SPAR-IDUSER-ENTER                   
076500        END-IF                                                            
076600                                                                          
076700        PERFORM UNTIL INDX > MAX-INDX                                     
076800          IF EOF-WDR5-NO                                                  
076900            IF IDPERSON-OK                                                
077000              MOVE 2404-IDARTNR    TO MOD-IDARTNR       (INDX)            
077100              MOVE 2404-IDDC       TO MOD-IDDC          (INDX)            
077200              MOVE 2404-IDFKNGRP   TO MOD-IDFKNGRP      (INDX)            
077300              MOVE 2404-BEART-ENG  TO MOD-BEART10       (INDX)            
077400              MOVE 2404-KDLEVSP    TO MOD-KDLEVSP       (INDX)            
077500              MOVE 2404-KVSPARR-KVAL                                      
077600                                   TO MOD-KVSPARR-KVAL  (INDX)            
077700              MOVE 2404-KVLS       TO MOD-KVLS          (INDX)            
077800              MOVE 2404-KVROS      TO MOD-KVROS         (INDX)            
077900              MOVE 2404-TISPARR-KVAL                                      
078000                                   TO MOD-TISPARR-KVAL  (INDX)            
078100              MOVE 2404-IDUSER-SPKVAL                                     
078200                                   TO MOD-IDUSER-SPKVAL (INDX)            
078300              ADD 1                TO INDX                                
078400            END-IF                                                        
078500            PERFORM FB-LAS-NAESTA                                         
078600            IF SEGMENT-FINNS                                              
078700              PERFORM FC-READ-WDP3                                        
078800            ELSE                                                          
078900              SET EOF-WDR5-YES     TO TRUE                                
079000            END-IF                                                        
079100          ELSE                                                            
079200            MOVE MFS-RENSA-FAELT TO MOD-IDARTNR       (INDX)              
079300                                    MOD-IDDC          (INDX)              
079400                                    MOD-IDFKNGRP      (INDX)              
079500                                    MOD-BEART10       (INDX)              
079600                                    MOD-KDLEVSP       (INDX)              
079700                                    MOD-KVSPARR-KVAL  (INDX)              
079800                                    MOD-KVLS          (INDX)              
079900                                    MOD-KVROS         (INDX)              
080000                                    MOD-TISPARR-KVAL  (INDX)              
080100                                    MOD-IDUSER-SPKVAL (INDX)              
080200            ADD 1 TO INDX                                                 
080300          END-IF                                                          
080400       END-PERFORM                                                        
080500                                                                          
080600       IF EOF-WDR5-NO                                                     
080700          MOVE 2404-IDARTNR       TO SPAR-IDARTNR-NEXT                    
080800          MOVE 2404-IDDC          TO SPAR-IDDC-NEXT                       
080900          MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                       
081000          CALL WMEDKONV USING MED-WMEDAREA                                
081100          MOVE MED-TEMFSINF TO MOD-TEMFSINF                               
081200       ELSE                                                               
081300          MOVE MFS-RENSA-FAELT TO SPAR-IDARTNR-NEXT                       
081400                                     SPAR-IDDC-NEXT                       
081500                                     SPAR-IDFKNGRP-NEXT                   
081600                                     SPAR-KDLEVSP-NEXT                    
081700                                     SPAR-KVSPARR-NEXT                    
081800                                     SPAR-IDUSER-NEXT                     
081900          MOVE ZERO TO SPAR-IDARTNR-NEXT                                  
082000       END-IF                                                             
082100                                                                          
082200       MOVE '002'      TO MSGI-KDCALL                                     
082300       MOVE '6216'   TO SPAR-IDTRANS                                      
082400       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
082500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
082600     END-IF                                                               
082700     .                                                                    
082800     EJECT                                                                
082900 FA-LAS-FOERSTA SECTION.                                                  
083000                                                                          
083100     PERFORM IMS-GU-WDGX2404                                              
083200     .                                                                    
083300     SKIP3                                                                
083400 FB-LAS-NAESTA SECTION.                                                   
083500                                                                          
083600     PERFORM IMS-GN-WDGX2404                                              
083700     .                                                                    
083800     EJECT                                                                
083900 FC-READ-WDP3 SECTION.                                                    
084000                                                                          
084100     SET IDPERSON-NOT-OK         TO TRUE                                  
084200                                                                          
084300     IF IDPERSON-SOEK = NEJ                                               
084400       SET IDPERSON-OK           TO TRUE                                  
084500     ELSE                                                                 
084600       MOVE 2404-IDARTNR         TO W-IDARTNR                             
084700       MOVE 2404-IDLEVNR         TO W-IDLEVNR                             
084800       MOVE 2404-IDFKNGRP        TO W-IDFKNGRP                            
084900       PERFORM IMS-GU-WDP311                                              
085000       IF SEGMENT-FINNS                                                   
085100         PERFORM IMS-GNP-WDP321                                           
085200         IF SEGMENT-FINNS                                                 
085300           SET IDPERSON-OK       TO TRUE                                  
085400         ELSE                                                             
085500           PERFORM IMS-GNP-WDP322                                         
085600           IF SEGMENT-FINNS                                               
085700             SET IDPERSON-OK     TO TRUE                                  
085800           ELSE                                                           
085900             PERFORM IMS-GNP-WDP323                                       
086000             IF SEGMENT-FINNS                                             
086100               SET IDPERSON-OK   TO TRUE                                  
086200             END-IF                                                       
086300           END-IF                                                         
086400         END-IF                                                           
086500       END-IF                                                             
086600     END-IF                                                               
086700     .                                                                    
086800     EJECT                                                                
086900 G-KOLLA-INDATA SECTION.                                                  
087000                                                                          
087100     MOVE JA TO INDATA-SW                                                 
087200                IDDC-SW                                                   
087300                                                                          
087400     MOVE MSGI-IDDC TO W-IDDC-B6                                          
087500     PERFORM IMS-GU-WDB601                                                
087600     IF DCS-NDC-NA                                                        
087700        MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                   
087800        PERFORM IMS-GU-WDB601                                             
087900        IF DCS-NDC-NA                                                     
088000        OR W-IDDC-B6 = '40'                                               
088100           CONTINUE                                                       
088200        ELSE                                                              
088300           MOVE NEJ TO IDDC-SW                                            
088400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDNODE-ATTR                     
088500           MOVE NEJ TO INDATA-SW                                          
088600        END-IF                                                            
088700     END-IF                                                               
088800     MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                      
088900     PERFORM IMS-GU-WDB601                                                
089000                                                                          
089100     IF MID-KDSORT1 NOT = ALL '+'                                         
089200        IF MID-KDSORT1 = '1' OR '2' OR '3'                                
089300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT1-ATTR                  
089400           MOVE MID-KDSORT1 TO WS-KDSORT1                                 
089500        ELSE                                                              
089600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT1-ATTR                    
089700           MOVE NEJ TO INDATA-SW                                          
089800        END-IF                                                            
089900     ELSE                                                                 
090000        MOVE ZERO TO WS-KDSORT1                                           
090100     END-IF                                                               
090200                                                                          
090300     IF MID-IDNODE = ALL '+'                                              
090400        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDNODE-ATTR                        
090500        MOVE NEJ TO INDATA-SW                                             
090600     ELSE                                                                 
090700        MOVE '004'             TO PRT-KDCALL                              
090800        MOVE MID-IDNODE        TO PRT-IDNODE                              
090900        CALL W006PRT USING PRT-W006PRT                                    
091000        IF PRT-KDSVAR = 'R'                                               
091100           MOVE MID-IDNODE     TO URV-IDPRINTER                           
091200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDNODE-ATTR                   
091300        ELSE                                                              
091400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDNODE-ATTR                     
091500           MOVE NEJ TO INDATA-SW                                          
091600        END-IF                                                            
091700     END-IF                                                               
091800                                                                          
091900     IF  INDATA-FEL                                                       
092000        IF IDDC-SW = NEJ                                                  
092100           MOVE PRINTING-NOT-ALLOWED-WRONG-DC TO MOD-TEMFSFEL             
092200        ELSE                                                              
092300           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
092400           CALL WMEDKONV USING MED-WMEDAREA                               
092500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
092600        END-IF                                                            
092700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
092800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
092900     END-IF                                                               
093000     .                                                                    
093100     EJECT                                                                
093200 H-STARTA-SOP SECTION.                                                    
093300                                                                          
093400     MOVE MSGI-IDDC         TO URV-IDDC-BEST                              
093500     MOVE WS-KDSORT1        TO URV-KDSORT1                                
093600     IF MSGI-IDDC-KEY = ZERO                                              
093700        MOVE SPACE          TO URV-IDDC                                   
093800     ELSE                                                                 
093900        MOVE MSGI-IDDC-KEY  TO URV-IDDC                                   
094000     END-IF                                                               
094100     IF MSGI-IDFKNGRP = SPACE                                             
094200        MOVE ZERO           TO URV-IDFKNGRP                               
094300     ELSE                                                                 
094400        MOVE MSGI-IDFKNGRP  TO URV-IDFKNGRP                               
094500     END-IF                                                               
094600     MOVE WS-IDUSER         TO URV-IDUSER                                 
094700     MOVE WS-KDLEVSP        TO URV-KDLEVSP                                
094800     IF WS-FLKVANT = 'Y'                                                  
094900        MOVE 'J'            TO URV-FLKVANT                                
095000     ELSE                                                                 
095100        MOVE WS-FLKVANT     TO URV-FLKVANT                                
095200     END-IF                                                               
095300     IF MSGI-IDPERSON = SPACE                                             
095400        MOVE ZERO           TO URV-IDPERSON                               
095500     ELSE                                                                 
095600        MOVE MSGI-IDPERSON  TO URV-IDPERSON                               
095700     END-IF                                                               
095800                                                                          
095900     MOVE '6216'   TO MSGSOP-IDTRANS                                      
096000     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
096100     MOVE 'W614S1' TO MSGSOP-IDPROCESS                                    
096200     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
096300                                                                          
096400     STRING 'URVAL(' WS-URVAL ')PRT('                                     
096500            WS-PRINTER ')'                                                
096600            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
096700                                                                          
096800     PERFORM IMS-INSERT-ALTMSG                                            
096900     MOVE INF-PRINT-BEGAERD TO MED-IDMFSINF                               
097000     CALL WMEDKONV USING MED-WMEDAREA                                     
097100     MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                    
097200     PERFORM MFS-RENSA-FAELT-IN                                           
097300     .                                                                    
097400     EJECT                                                                
097500 MFS-RENSA-FAELT-UT SECTION.                                              
097600                                                                          
097700*    --- ALLA UTDATA-FÄLT                                                 
097800*    --- INKL. BLÄDDRINGSNYCKLAR                                          
097900     MOVE +1 TO INDX                                                      
098000     PERFORM UNTIL INDX > MAX-INDX                                        
098100       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
098200       ADD +1 TO INDX                                                     
098300     END-PERFORM                                                          
098400     .                                                                    
098500     SKIP3                                                                
098600 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
098700                                                                          
098800*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
098900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR       (INDX)                     
099000                             MOD-IDDC          (INDX)                     
099100                             MOD-IDFKNGRP      (INDX)                     
099200                             MOD-BEART10       (INDX)                     
099300                             MOD-KDLEVSP       (INDX)                     
099400                             MOD-KVSPARR-KVAL  (INDX)                     
099500                             MOD-KVLS          (INDX)                     
099600                             MOD-KVROS         (INDX)                     
099700                             MOD-TISPARR-KVAL  (INDX)                     
099800                             MOD-IDUSER-SPKVAL (INDX)                     
099900     .                                                                    
100000     SKIP3                                                                
100100 MFS-RENSA-FAELT-IN SECTION.                                              
100200                                                                          
100300*    --- ALLA INDATA-FÄLT                                                 
100400     MOVE MFS-RENSA-FAELT TO MOD-KDSORT1                                  
100500                             MOD-IDNODE                                   
100600     .                                                                    
100700     EJECT                                                                
100800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
100900                                                                          
101000*    --- ALLA UTDATA-FÄLT                                                 
101100*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
101200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT1                                
101300                               MOD-IDNODE                                 
101400     MOVE +1 TO INDX                                                      
101500     PERFORM UNTIL INDX > MAX-INDX                                        
101600       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
101700       ADD +1 TO INDX                                                     
101800     END-PERFORM                                                          
101900     .                                                                    
102000     SKIP3                                                                
102100 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
102200                                                                          
102300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
102400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR       (INDX)                   
102500                               MOD-IDDC          (INDX)                   
102600                               MOD-IDFKNGRP      (INDX)                   
102700                               MOD-BEART10       (INDX)                   
102800                               MOD-KDLEVSP       (INDX)                   
102900                               MOD-KVSPARR-KVAL  (INDX)                   
103000                               MOD-KVLS          (INDX)                   
103100                               MOD-KVROS         (INDX)                   
103200                               MOD-TISPARR-KVAL  (INDX)                   
103300                               MOD-IDUSER-SPKVAL (INDX)                   
103400     .                                                                    
103500     SKIP3                                                                
103600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
103700                                                                          
103800*    --- ALLA INDATA-FÄLT                                                 
103900     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT1                                
104000                               MOD-IDNODE                                 
104100     .                                                                    
104200     EJECT                                                                
104300 MFS-RENSA-SPAR-NYCKLAR SECTION.                                          
104400                                                                          
104500     MOVE MFS-RENSA-FAELT TO SPAR-IDFKNGRP-ENTER                          
104600                             SPAR-IDFKNGRP-NEXT                           
104700                             SPAR-IDARTNR-ENTER                           
104800                             SPAR-IDARTNR-NEXT                            
104900                             SPAR-IDDC-ENTER                              
105000                             SPAR-IDDC-NEXT                               
105100                             SPAR-KDLEVSP-ENTER                           
105200                             SPAR-KDLEVSP-NEXT                            
105300                             SPAR-IDUSER-ENTER                            
105400                             SPAR-IDUSER-NEXT                             
105500                             SPAR-KVSPARR-ENTER                           
105600                             SPAR-KVSPARR-NEXT                            
105700     .                                                                    
105800     EJECT                                                                
105900* --- IMS SEKTIONER ---                                                   
106000     SKIP3                                                                
106100 IMS-GET-MSG SECTION.                                                     
106200     MOVE '  QC' TO GODK-STATUSKODER                                      
106300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
106400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     SKIP3                                                                
106800 IMS-INSERT-MSG SECTION.                                                  
106900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
107000     MOVE SPACE TO GODK-STATUSKODER                                       
107100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
107200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500     SKIP3                                                                
107600 IMS-INSERT-ALTMSG SECTION.                                               
107700     MOVE SPACE TO GODK-STATUSKODER                                       
107800     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
107900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     EJECT                                                                
108300 IMS-GET-WDR501 SECTION.                                                  
108400     STRING 'WDR501  (WDGXKEY  =' W-2403KEY-X ')'                         
108500          DELIMITED BY SIZE INTO SSA1                                     
108600     MOVE 'GE' TO GODK-STATUSKODER                                        
108700     CALL CBLTDLI USING GU 2404-PCB DLI-IO-WDGX2404 SSA1                  
108800     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
108900     PERFORM IMS-STATUSKONTROLL                                           
109000     .                                                                    
109100     SKIP3                                                                
109200 IMS-GU-WDGX2404 SECTION.                                                 
109300     STRING 'WDGX2404(KY2404  =>' W-2404KEY-MIN-X                         
109400                    '&KY2404  =<' W-2404KEY-MAX-X                         
109500                    '&IDDC    =>' W-IDDC-MIN-NYCKEL-X                     
109600                    '&IDDC    =<' W-IDDC-MAX-X                            
109700                    '&IDFKNGRP=>' W-IDFKNGRP-MIN-X                        
109800                    '&IDFKNGRP=<' W-IDFKNGRP-MAX-X                        
109900                    '&KDLEVSP =>' W-KDLEVSP-MIN-X                         
110000                    '&KDLEVSP =<' W-KDLEVSP-MAX-X                         
110100                    '&KVSPARR =>' W-KVSPARR-MIN-X                         
110200                    '&KVSPARR =<' W-KVSPARR-MAX-X                         
110300                    '&IDUSER  =>' W-IDUSER-MIN-X                          
110400                    '&IDUSER  =<' W-IDUSER-MAX-X ')'                      
110500           DELIMITED BY SIZE INTO SSA1                                    
110600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
110700     CALL CBLTDLI USING GN 2404-PCB DLI-IO-WDGX2404 SSA1                  
110800     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
110900     PERFORM IMS-STATUSKONTROLL                                           
111000     .                                                                    
111100     EJECT                                                                
111200 IMS-GN-WDGX2404 SECTION.                                                 
111300     STRING 'WDGX2404(KY2404  =>' W-2404KEY-MIN-X                         
111400                    '&KY2404  =<' W-2404KEY-MAX-X                         
111500                    '&IDDC    =>' W-IDDC-MIN-X                            
111600                    '&IDDC    =<' W-IDDC-MAX-X                            
111700                    '&IDFKNGRP=>' W-IDFKNGRP-MIN-X                        
111800                    '&IDFKNGRP=<' W-IDFKNGRP-MAX-X                        
111900                    '&KDLEVSP =>' W-KDLEVSP-MIN-X                         
112000                    '&KDLEVSP =<' W-KDLEVSP-MAX-X                         
112100                    '&KVSPARR =>' W-KVSPARR-MIN-X                         
112200                    '&KVSPARR =<' W-KVSPARR-MAX-X                         
112300                    '&IDUSER  =>' W-IDUSER-MIN-X                          
112400                    '&IDUSER  =<' W-IDUSER-MAX-X ')'                      
112500          DELIMITED BY SIZE INTO SSA1                                     
112600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
112700     CALL CBLTDLI USING GN 2404-PCB DLI-IO-WDGX2404 SSA1                  
112800     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     EJECT                                                                
113200 IMS-GU-WDB601    SECTION.                                                
113300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     MOVE '  GE' TO GODK-STATUSKODER                                      
113600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
113700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
113800     PERFORM IMS-STATUSKONTROLL                                           
113900     IF SEGMENT-SAKNAS                                                    
114000         MOVE SPACE TO DCS-KDDC                                           
114100     END-IF                                                               
114200     .                                                                    
114300 IMS-GU-WDP311    SECTION.                                                
114400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
114500          DELIMITED BY SIZE INTO SSA1                                     
114600     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
114700          DELIMITED BY SIZE INTO SSA2                                     
114800     MOVE '  GE' TO GODK-STATUSKODER                                      
114900     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
115000     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
115100     PERFORM IMS-STATUSKONTROLL                                           
115200     .                                                                    
115300 IMS-GNP-WDP321    SECTION.                                               
115400     STRING 'WDP321  (WDP321KY=>' W-WDP321KY-MIN-X                        
115500                    '&WDP321KY=<' W-WDP321KY-MAX-X                        
115600                    '&IDARTNRF=<' W-IDARTNR-X                             
115700                    '&IDARTNRT=>' W-IDARTNR-X      ')'                    
115800          DELIMITED BY SIZE INTO SSA1                                     
115900     MOVE '  GE' TO GODK-STATUSKODER                                      
116000     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP321 SSA1                   
116100     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
116200     PERFORM IMS-STATUSKONTROLL                                           
116300     .                                                                    
116400 IMS-GNP-WDP322    SECTION.                                               
116500     STRING 'WDP322  (WDP322KY =' W-WDP322KY-X ')'                        
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     MOVE '  GE' TO GODK-STATUSKODER                                      
116800     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP322 SSA1                   
116900     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
117000     PERFORM IMS-STATUSKONTROLL                                           
117100     .                                                                    
117200 IMS-GNP-WDP323    SECTION.                                               
117300     STRING 'WDP323  (WDP323KY=>' W-WDP323KY-MIN-X                        
117400                    '&WDP323KY=<' W-WDP323KY-MAX-X                        
117500                    '&IDFKNGRF=<' W-IDFKNGRP-X                            
117600                    '&IDFKNGRT=>' W-IDFKNGRP-X     ')'                    
117700          DELIMITED BY SIZE INTO SSA1                                     
117800     MOVE '  GE' TO GODK-STATUSKODER                                      
117900     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP323 SSA1                   
118000     MOVE WDP3-STATUS-CODE    TO STATUS-WS                                
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     .                                                                    
118300 IMS-STATUSKONTROLL SECTION.                                              
118400     SET STATUS-IX TO 1                                                   
118500     SEARCH GODK-STATUS                                                   
118600       AT END                                                             
118700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
118800         DELIMITED BY SIZE INTO FELTEXT                                   
118900         CALL FELLOG                                                      
119000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
119100         CONTINUE                                                         
119200     END-SEARCH                                                           
119300     .                                                                    
