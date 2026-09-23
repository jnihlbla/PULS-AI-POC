000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4035400.                                                
000400 AUTHOR.         ROGER OLSSON.                                            
000500 DATE-WRITTEN.   90/11/09.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        VISAR SATSORDER SOM ÄR BYGGBARA OCH TILLHÖR VISS PRC.            
001100*        EN ORDER SOM MARKERAS MED 'X' BILDAR EN PLOCKSATS.               
001200*        OM MAN TRYCKER PF4 FÅR MAN DEN 1:A ORDERN PÅ KÖN,                
001300*        ANNARS PF11 FÖR KRYSSMÄRKT ORDER.                                
001400*                                                                         
001500*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001600*        PROGRAMMET UPPDATERAR WLSATG (WDJ2)                              
001700*                              WLARTC (WDK6)                              
001800*                              WLORDP (WDA5)                              
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T354                                              
002200*        MID:         W4I35401                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O35401                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200*    -- CHECKED BY WY2000                                                 
003300     SKIP3                                                                
003400 77  IDPGM                       PIC X(08)   VALUE 'W4035400'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004500 77  X-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  MIN-MOD-LAENGD              PIC S9(4)  VALUE +150  COMP SYNC.        
004800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +941  COMP SYNC.        
004900 77  MAX-VARDE                   PIC  9(5)  VALUE 99999.                  
005000 77  FILLER                      PIC  X(8)  VALUE 'AAAAAAAA'.             
005100 77  ANTAL-ISRT-2191             PIC S9(3)  VALUE +0.                     
005200                                                                          
005300 77  WS-TRAEFF                   PIC  X(1).                               
005400     88 TRAEFF                   VALUE 'J'.                               
005500 77  WS-DATUM                    PIC  9(8).                               
005600 77  WS-TIME                     PIC  X(8).                               
005700 77  WS-KVRORAD                  PIC S9(5)  COMP-3.                       
005800 77  WS-KVART                    PIC S9(7)  COMP-3.                       
005900 77  WS-KVRO                     PIC S9(7)  COMP-3.                       
006000 77  WS-KVROS                    PIC S9(7)  COMP-3.                       
006100 77  WS-DISPSALDO                PIC S9(7)  COMP-3.                       
006200 77  WS-KVBYGGB                  PIC S9(7)  COMP-3.                       
006300 77  WS-KVBYGGB-SPAR             PIC S9(7)  COMP-3.                       
006400 77  WS-KVBYGGB-SATKMB           PIC S9(7)  COMP-3.                       
006500 77  WS-KVSATRES                 PIC S9(7)  COMP-3.                       
006600 77  WS-KVOT-TIHH                PIC S9(3)  COMP-3.                       
006700 77  WS-REST-TIMM                PIC S9(3)  COMP-3.                       
006800 77  WS-TIHH-ACK                 PIC S9(5)  COMP-3 VALUE 0.               
006900 77  WS-TIMM-ACK                 PIC S9(3)  COMP-3 VALUE 0.               
007000 77  WS-SPAR-IDARTNR             PIC S9(9)  COMP-3 VALUE 0.               
007100 77  WS-SPAR-KDSATKMB            PIC  X(1).                               
007200 77  WS-IDDISTR-NUM4             PIC  9(4).                               
007300 77  WS-IDKUNDNR-NUM6            PIC  9(6).                               
007400                                                                          
007500 01  WS-TIHHMM                   PIC  9(5)V99  VALUE 0.                   
007600 01  FILLER REDEFINES WS-TIHHMM.                                          
007700     03  WS-TIHH                 PIC  9(5).                               
007800     03  WS-TIMM                 PIC  9(2).                               
007900                                                                          
008000                                                                          
008100*****************************************************                     
008200*    --- TABELL ÖVER KODER VARFÖR RADEN RESTNOTERAS                       
008300*****************************************************                     
008400*                                                                         
008500*   1 = BRIST VID LAGERAVBKNING                                           
008600*   2 = KVALITETSSPÄRR                                                    
008700*   3 = STANDARDPRIS SAKNAS                                               
008800*   4 = FYSISK AVVIKELSE                                                  
008900*   5 = DO-BEHOV PRIORITERAS                                              
009000*   6 = ODEFINERAT I DM                                                   
009100*   7 = ODEFINERAT I DM                                                   
009200*   8 = ODEFINERAT I DM                                                   
009300*   9 = ÖVRIGA ORSAKER                                                    
009400*****************************************************                     
009500                                                                          
009600 77  WS-KDROO-1                  PIC 9(1)   VALUE 1.                      
009700 77  WS-KDROO-2                  PIC 9(1)   VALUE 2.                      
009800 77  WS-KDROO-3                  PIC 9(1)   VALUE 3.                      
009900 77  WS-KDROO-4                  PIC 9(1)   VALUE 4.                      
010000 77  WS-KDROO-5                  PIC 9(1)   VALUE 5.                      
010100 77  WS-KDROO-6                  PIC 9(1)   VALUE 6.                      
010200 77  WS-KDROO-7                  PIC 9(1)   VALUE 7.                      
010300 77  WS-KDROO-8                  PIC 9(1)   VALUE 8.                      
010400 77  WS-KDROO-9                  PIC 9(1)   VALUE 9.                      
010500                                                                          
010600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010700                                                                          
010800 77  WS-IDPRC                    PIC X(4).                                
010900 77  WS-IDUSER                   PIC X(8).                                
011000                                                                          
011100 01  FILLER                      PIC X(8)    VALUE 'SWITCHAR'.            
011200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
011300     88  INDATA-OK                           VALUE 'J'.                   
011400     88  INDATA-FEL                          VALUE 'N'.                   
011500                                                                          
011600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011700     88  NYCKLAR-OK                          VALUE 'J'.                   
011800     88  NYCKLAR-FEL                         VALUE 'N'.                   
011900                                                                          
012000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
012100     88  ALLT-OK                             VALUE 'J'.                   
012200                                                                          
012300 77  BYGGBAR-SW                  PIC X       VALUE 'N'.                   
012400     88  SATS-BYGGBAR                        VALUE 'J'.                   
012500     88  SATS-EJ-BYGGBAR                     VALUE 'N'.                   
012600                                                                          
012700 77  CLAGER-SW                   PIC X       VALUE 'N'.                   
012800     88  CLAGER-SPAERR                       VALUE 'J'.                   
012900                                                                          
013000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013100     88  EGEN-MID                            VALUE '4354'.                
013200     88  GODK-MID                            VALUE '4354'.                
013300     EJECT                                                                
013400*      --- VALID IDDC CODES                                               
013500*                                                                         
013600*01    -COPY WWDCKONS                                                     
013700       EJECT                                                              
013800*                                                                         
013900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014000 01  GENERELLA-SUBPROGRAM.                                                
014100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014400     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
014500     EJECT                                                                
014600*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
014700*   -COPY W411ORDN                                                        
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015000*   -COPY WMEDAREA                                                        
015100     SKIP3                                                                
015200 01  MESSAGE-CODES.                                                       
015300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
015400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
015500     03  INF-LAST-PAGE-SHOW      PIC X(3)    VALUE '115'.                 
015600     03  INF-PRESS-PF11          PIC X(3)    VALUE '168'.                 
015700     03  INF-NO-INFO-EXISTS      PIC X(3)    VALUE '413'.                 
015800     03  INF-KIT-NOT-FILLED      PIC X(3)    VALUE '039'.                 
015900     03  ERR-CORR-FIELDS         PIC X(3)    VALUE '001'.                 
016000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016100     EJECT                                                                
016200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016300*                                                                         
016400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016500     SKIP3                                                                
016600*01  MID -COPY W4I35401                                                   
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016900     SKIP3                                                                
017000*01  -COPY WMSGAREA                                                       
017100     EJECT                                                                
017200     03  MOD REDEFINES MSG-AREA.                                          
017300*      05  -COPY W4O35401                                                 
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'MID-W4I37101'.        
017600                                                                          
017700 01  MOD-MID-W4I37101.                                                    
017800*    03   -COPY W4I35401    -PRE MOD-                                     
017900     EJECT                                                                
018000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018100     SKIP3                                                                
018200*01  -COPY WMFSAREA                                                       
018300     EJECT                                                                
018400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018500*                                                                         
018600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018700     SKIP3                                                                
018800 01  NYCKLAR-TILL-DLI.                                                    
018900                                                                          
019000*----> DIREKTNYCKEL TILL SATSORDER,                                       
019100                                                                          
019200     03  W-WDJ2-IDORDNST-X.                                               
019300         05 W-WDJ2-IDORDNSB               PIC S9(5) COMP-3.               
019400         05 W-WDJ2-IDORDNSS               PIC S9(1) COMP-3.               
019500                                                                          
019600*----> DIREKTNYCKEL TILL SATSRAD,                                         
019700                                                                          
019800     03  W-WDJ2-IDARTNR-X.                                                
019900         05 W-WDJ2-IDARTNR                PIC S9(9) COMP-3.               
020000                                                                          
020100*----> SEKUNDÄR INDEX TILL SATSORDERKÖN.                                  
020200                                                                          
020300     03  W-WDJ2ASEQ-MIN-X.                                                
020400         05 W-WDJ2ASEQ-MIN-KDCLAGER       PIC S9(1)  COMP-3.              
020500         05 W-WDJ2ASEQ-MIN-IDPRC          PIC  X(4).                      
020600         05 W-WDJ2ASEQ-MIN-KVRORAD-9KOMPL PIC S9(5)      COMP-3.          
020700         05 W-WDJ2ASEQ-MIN-RELSKVOT-L     PIC S9(3)V9(2) COMP-3.          
020800         05 W-WDJ2ASEQ-MIN-FLSATPRI       PIC  X(1).                      
020900         05 W-WDJ2ASEQ-MIN-RELSKVOT-S     PIC S9(3)V9(2) COMP-3.          
021000         05 W-WDJ2ASEQ-MIN-IDARTNR        PIC S9(9)      COMP-3.          
021100         05 W-WDJ2ASEQ-MIN-DAREGDAT       PIC  9(8).                      
021200                                                                          
021300     03  W-WDJ2ASEQ-MAX-X.                                                
021400         05 W-WDJ2ASEQ-MAX-KDCLAGER       PIC S9(1)      COMP-3.          
021500         05 W-WDJ2ASEQ-MAX-IDPRC          PIC  X(4).                      
021600         05 W-WDJ2ASEQ-MAX-KVRORAD-9KOMPL PIC S9(5)      COMP-3.          
021700         05 W-WDJ2ASEQ-MAX-RELSKVOT-L     PIC S9(3)V9(2) COMP-3.          
021800         05 W-WDJ2ASEQ-MAX-FLSATPRI       PIC  X(1).                      
021900         05 W-WDJ2ASEQ-MAX-RELSKVOT-S     PIC S9(3)V9(2) COMP-3.          
022000         05 W-WDJ2ASEQ-MAX-IDARTNR        PIC S9(9)      COMP-3.          
022100         05 W-WDJ2ASEQ-MAX-DAREGDAT       PIC  9(8).                      
022200                                                                          
022300*----> ARTIKELREGISTER WDK6.                                              
022400                                                                          
022500     03  W-IDARTNR-X.                                                     
022600         05 W-IDARTNR       PIC S9(9)      COMP-3.                        
022700                                                                          
022800     03  W-WDK6-KDSEGKEY-X       PIC X(1)       VALUE '1'.                
022900                                                                          
023000*----> PRIORITETSSTYRNING FÖR RESTORDER                                   
023100                                                                          
023200     03  W-4511-IDHTYP-X.                                                 
023300         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
023400         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
023500                                                                          
023600     03  W-4512-KDTPOTYP-X.                                               
023700         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
023800     03  W-4512-KDORDKL-X.                                                
023900         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
024000     03  W-4512-IDDISTR-FOM-X.                                            
024100         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
024200     03  W-4512-IDDISTR-TOM-X.                                            
024300         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
024400                                                                          
024500 01  FILLER                    PIC X(16) VALUE 'ALT2191-IO-AREA'.         
024600 01  ALT2191-IO-AREA.                                                     
024700  03     ALT2191-LL               PIC S9(4) COMP SYNC.                    
024800  03     ALT2191-Z1               PIC X(1)  VALUE LOW-VALUE.              
024900  03     ALT2191-Z2               PIC X(1)  VALUE LOW-VALUE.              
025000  03     ALT2191-TRANSKOD         PIC X(8)  VALUE 'W2T191X '.             
025100  03     ALT2191-IDTRANS          PIC X(4)  VALUE '4354'.                 
025200  03     ALT2191-SPRAK            PIC X(1).                               
025300* 03     MID -COPY W2I19101   -PRE ALT2191-                               
025400     EJECT                                                                
025500*    --- STATUS-KOD FRÅN IMS                                              
025600 01  STATUS-WS                   PIC XX.                                  
025700     88  SEGMENT-FINNS                       VALUE '  '.                  
025800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026000     88  END-OF-DATA                         VALUE 'GB'.                  
026100     SKIP2                                                                
026200 01  GODK-STATUSKODER.                                                    
026300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026400     SKIP3                                                                
026500 01  SSA1                        PIC X(96).                               
026600 01  SSA2                        PIC X(96).                               
026700 01  SSA3                        PIC X(96).                               
026800     EJECT                                                                
026900*    --- IMS FUNKTIONSKODER                                               
027000*01  -COPY W0003                                                          
027100     EJECT                                                                
027200*    ---  DLI INPUT-OUTPUT AREA                                           
027300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027400     SKIP3                                                                
027500 01  DLI-IO-AREA1.                                                        
027600     03 WLSATG01.                                                         
027700*    05  -COPY WDJ201                                                     
027800     EJECT                                                                
027900 01  DLI-IO-AREA2.                                                        
028000     03 WLSATG11.                                                         
028100*    05  -COPY WDJ211                                                     
028200     EJECT                                                                
028300 01  DLI-IO-AREA3.                                                        
028400     03 WLARTC11.                                                         
028600*    05  -COPY WDK611                                                     
028700     EJECT                                                                
028800 01  DLI-IO-AREA4.                                                        
028900     03 IO-AREA4       PIC X(260).                                        
029000     03 WLXXJN11 REDEFINES IO-AREA4.                                      
029100*    05  -COPY WDGX4512                                                   
029200     EJECT                                                                
029300 01  DLI-IO-AREA5.                                                        
029400     03 IO-AREA5       PIC X(300).                                        
029500     03 WLORDP01 REDEFINES IO-AREA5.                                      
029600*    05  -COPY WDA501     -PRE RO-                                        
029700     EJECT                                                                
029800     EJECT                                                                
029900 LINKAGE SECTION.                                                         
030000                                                                          
030100*01  -COPY W0009      -PRE MSG-                                           
030200     EJECT                                                                
030300*01  -COPY W0009      -PRE ALT-                                           
030400     EJECT                                                                
030500*01  -COPY W0009      -PRE ALT2191-                                       
030600     EJECT                                                                
030700*01  -COPY W0008      -PRE SATG-                                          
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008      -PRE SATH-                                          
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008      -PRE ARTC-                                          
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031600*01  -COPY W0008      -PRE ORDP-                                          
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900*01  -COPY W0008      -PRE XXJN-                                          
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200 01  ORDN-XXKP-PCB               PIC X(1).                                
032300 01  ORDN-ORQL-PCB               PIC X(1).                                
032400 01  ORDN-PROC-PCB               PIC X(1).                                
032500 01  ORDN-ORQI-PCB               PIC X(1).                                
032600 01  ORDN-WDQ3-PCB               PIC X(1).                                
032700     EJECT                                                                
032800 PROCEDURE DIVISION  USING MSG-PCB                                        
032900                           ALT-PCB                                        
033000                           ALT2191-PCB                                    
033100                           SATG-PCB                                       
033200                           SATH-PCB                                       
033300                           ARTC-PCB                                       
033400                           ORDP-PCB                                       
033500                           XXJN-PCB                                       
033600                           ORDN-XXKP-PCB                                  
033700                           ORDN-ORQL-PCB                                  
033800                           ORDN-PROC-PCB                                  
033900                           ORDN-ORQI-PCB                                  
034000                           ORDN-WDQ3-PCB.                                 
034100                                                                          
034200     ENTRY 'DLITCBL' USING MSG-PCB                                        
034300                           ALT-PCB                                        
034400                           ALT2191-PCB                                    
034500                           SATG-PCB                                       
034600                           SATH-PCB                                       
034700                           ARTC-PCB                                       
034800                           ORDP-PCB                                       
034900                           XXJN-PCB                                       
035000                           ORDN-XXKP-PCB                                  
035100                           ORDN-ORQL-PCB                                  
035200                           ORDN-PROC-PCB                                  
035300                           ORDN-ORQI-PCB                                  
035400                           ORDN-WDQ3-PCB.                                 
035500                                                                          
035600     PERFORM IMS-GET-MSG                                                  
035700     IF SEGMENT-FINNS                                                     
035800        PERFORM A-INIT                                                    
035900        PERFORM B-KOLLA-NYCKLAR                                           
036000        IF NYCKLAR-OK                                                     
036100           IF MFS-UPDATE                                                  
036200              PERFORM G-KOLLA-INPUT                                       
036300              IF INDATA-OK                                                
036400                 PERFORM H-UPPDATERA                                      
036500              END-IF                                                      
036600           ELSE                                                           
036700              IF MFS-FIRST                                                
036800                 PERFORM C-FOERSTA-SIDA                                   
036900              ELSE                                                        
037000                 IF MFS-NEXT                                              
037100                    PERFORM D-NAESTA-SIDA                                 
037200                 ELSE                                                     
037300                    IF MFS-PRINT                                          
037400                       PERFORM J-KOLLA-PRINT                              
037500                    ELSE                                                  
037600                       PERFORM E-SAMMA-SIDA                               
037700                    END-IF                                                
037800                 END-IF                                                   
037900              END-IF                                                      
038000           END-IF                                                         
038100           IF ALLT-OK                                                     
038200              PERFORM F-LAES-VISA-INFO                                    
038300           END-IF                                                         
038400       END-IF                                                             
038500       PERFORM I-SKICKA-IMSTRANS                                          
038600     END-IF                                                               
038700                                                                          
038800     MOVE ZERO TO RETURN-CODE                                             
038900     GOBACK                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 A-INIT SECTION.                                                          
039300                                                                          
039400     IF MSG-DUBBLA-TRANSKODER                                             
039500        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35401                
039600        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
039700        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
039800     ELSE                                                                 
039900        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I35401                 
040000        MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                  
040100        MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                 
040200     END-IF                                                               
040300                                                                          
040400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
040500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
040600     MOVE MFS-IDTRANS          TO W-IDTRANS                               
040700                                                                          
040800     MOVE LOW-VALUE       TO MSG-AREA                                     
040900     MOVE 'W4O354N1'      TO MFS-IDMOD                                    
041000     MOVE '4354'          TO MOD-IDTRANS                                  
041100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
041200                                                                          
041300     IF NOT EGEN-MID                                                      
041400        MOVE SPACE TO MFS-KDTRTYP                                         
041500        MOVE '7'   TO MFS-IDPFK                                           
041600     END-IF                                                               
041700                                                                          
041800     IF ENGLISH-TEXT                                                      
041900        MOVE +2    TO SPRAK-IX                                            
042000        MOVE 'GB ' TO MED-IDSKYLT                                         
042100     ELSE                                                                 
042200        MOVE +1    TO SPRAK-IX                                            
042300        MOVE 'S  ' TO MED-IDSKYLT                                         
042400     END-IF                                                               
042500                                                                          
042600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
042700     ACCEPT WS-TIME  FROM TIME                                            
042800                                                                          
042900     MOVE ZERO                 TO ANTAL-ISRT-2191                         
043000     MOVE MID-SUACKPTI-ENTER   TO MOD-SUACKPTI-ENTER                      
043100     MOVE MID-SUACKPTI-NEXT    TO MOD-SUACKPTI-NEXT                       
043200     MOVE MID-IDORDNSB-BYGGBAR TO MOD-IDORDNSB-BYGGBAR                    
043300     MOVE MID-IDORDNSS-BYGGBAR TO MOD-IDORDNSS-BYGGBAR                    
043400     .                                                                    
043500     EJECT                                                                
043600 B-KOLLA-NYCKLAR SECTION.                                                 
043700                                                                          
043800     MOVE JA TO NYCKLAR-SW                                                
043900                                                                          
044000     MOVE LOW-VALUE  TO W-WDJ2ASEQ-MIN-X                                  
044100     MOVE HIGH-VALUE TO W-WDJ2ASEQ-MAX-X                                  
044200                                                                          
044300                             MOD-IDPRC-IN                                 
044400                             MOD-IDUSER-IN                                
044500                                                                          
044600                                                                          
044700     IF MID-IDPRC-IN = ALL '+'                                            
044800        MOVE MID-IDPRC-UT TO WS-IDPRC                                     
044900     ELSE                                                                 
045000        MOVE MID-IDPRC-IN TO WS-IDPRC                                     
045100        MOVE '7'          TO MFS-IDPFK                                    
045200        MOVE SPACE        TO MFS-KDTRTYP                                  
045300     END-IF                                                               
045400                                                                          
045500     INSPECT WS-IDPRC REPLACING LEADING SPACE BY ZERO                     
045600                                                                          
045700     IF WS-IDPRC (1:3) NOT NUMERIC                                        
045800        OR                                                                
045900        WS-IDPRC (1:3) = ZERO                                             
046000        MOVE NEJ TO NYCKLAR-SW                                            
046100     END-IF                                                               
046200                                                                          
046300     IF MID-IDUSER-IN = ALL '+'                                           
046400        MOVE MID-IDUSER-UT TO WS-IDUSER                                   
046500     ELSE                                                                 
046600        MOVE MID-IDUSER-IN TO WS-IDUSER                                   
046700     END-IF                                                               
046800                                                                          
046900     INSPECT WS-IDUSER REPLACING LEADING SPACE BY ZERO                    
047000                                                                          
047100     IF MFS-UPDATE                                                        
047200        IF WS-IDUSER NOT NUMERIC                                          
047300           OR                                                             
047400           WS-IDUSER (1:3) NOT = ZERO                                     
047500           OR                                                             
047600           WS-IDUSER (4:5)     = ZERO                                     
047700           MOVE NEJ TO NYCKLAR-SW                                         
047800        END-IF                                                            
047900     END-IF                                                               
048000                                                                          
048100*    IF NOT GODK-MID                                                      
048200*       MOVE NEJ TO NYCKLAR-SW                                            
048300*    END-IF                                                               
048400                                                                          
048500     IF GODK-MID OR NYCKLAR-OK                                            
048600        MOVE WS-IDPRC    TO MOD-IDPRC-UT                                  
048700        MOVE WS-IDUSER   TO MOD-IDUSER-UT                                 
048800        INSPECT MOD-IDPRC-UT  REPLACING LEADING ZERO BY SPACE             
048900        INSPECT MOD-IDUSER-UT REPLACING LEADING ZERO BY SPACE             
049000     ELSE                                                                 
049100        MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UT                              
049200                                MOD-IDUSER-UT                             
049300     END-IF                                                               
049400                                                                          
049500     IF NYCKLAR-FEL                                                       
049600        IF GODK-MID                                                       
049700           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
049800           PERFORM S01-FEL-MEDDELANDE                                     
049900*          PERFORM MFS-RENSA-FAELT-UT                                     
050000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
050100           PERFORM MFS-LAES-IN-IGEN                                       
050200        END-IF                                                            
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 C-FOERSTA-SIDA SECTION.                                                  
050700                                                                          
050800     MOVE ZERO        TO MOD-SUACKPTI-ENTER                               
050900                         MOD-SUACKPTI-NEXT                                
051000                         MOD-IDORDNSB-BYGGBAR                             
051100                         MOD-IDORDNSS-BYGGBAR                             
051200     MOVE +1          TO W-WDJ2ASEQ-MIN-KDCLAGER                          
051300                         W-WDJ2ASEQ-MAX-KDCLAGER                          
051400     MOVE WS-IDPRC    TO W-WDJ2ASEQ-MIN-IDPRC                             
051500                         W-WDJ2ASEQ-MAX-IDPRC                             
051600                                                                          
051700     PERFORM IMS-GN-WDJ2-WLSATG01                                         
051800                                                                          
051900     PERFORM UNTIL SEGMENT-SAKNAS                                         
052000                   OR                                                     
052100                   END-OF-DATA                                            
052200                   OR                                                     
052300                   SATS-BYGGBAR                                           
052400        MOVE SHUV-IDORDNSB TO W-WDJ2-IDORDNSB                             
052500        MOVE SHUV-IDORDNSS TO W-WDJ2-IDORDNSS                             
052600        PERFORM S04-KOLLA-BYGGBAR                                         
052700        IF SATS-EJ-BYGGBAR                                                
052800           PERFORM IMS-GN-WDJ2-WLSATG01                                   
052900        END-IF                                                            
053000     END-PERFORM                                                          
053100                                                                          
053200     IF SATS-BYGGBAR                                                      
053300        MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                               
053400        PERFORM S01-FEL-MEDDELANDE                                        
053500     ELSE                                                                 
053600        MOVE INF-NO-INFO-EXISTS TO MED-IDMFSINF                           
053700        PERFORM S02-INFO-MEDDELANDE                                       
053800        PERFORM MFS-RENSA-FAELT-UT                                        
053900        MOVE NEJ TO ALLT-SW                                               
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 D-NAESTA-SIDA SECTION.                                                   
054400                                                                          
054500     MOVE MOD-SUACKPTI-NEXT TO WS-TIHHMM                                  
054600     MOVE WS-TIHH           TO WS-TIHH-ACK                                
054700     MOVE WS-TIMM           TO WS-TIMM-ACK                                
054800                                                                          
054900     PERFORM DA-SKAPA-NYCKEL-NEXT                                         
055000                                                                          
055100     PERFORM S03-SKAPA-INDEXNYCKEL                                        
055200                                                                          
055300     IF NOT ALLT-OK                                                       
055400        MOVE INF-LAST-PAGE-SHOW TO MED-IDMFSFEL                           
055500        PERFORM S01-FEL-MEDDELANDE                                        
055600        PERFORM MFS-RENSA-FAELT-UT                                        
055700        MOVE MAX-VARDE TO MOD-IDORDNSB-ENTER                              
055800                          MOD-IDORDNSS-ENTER                              
055900                          MOD-IDORDNSB-NEXT                               
056000                          MOD-IDORDNSS-NEXT                               
056100     END-IF                                                               
056200                                                                          
056300     .                                                                    
056400     EJECT                                                                
056500 DA-SKAPA-NYCKEL-NEXT SECTION.                                            
056600                                                                          
056700     IF MID-IDORDNSB-NEXT NUMERIC                                         
056800        MOVE MID-IDORDNSB-NEXT TO W-WDJ2-IDORDNSB                         
056900     ELSE                                                                 
057000        MOVE ZERO              TO W-WDJ2-IDORDNSB                         
057100     END-IF                                                               
057200                                                                          
057300     IF MID-IDORDNSS-NEXT NUMERIC                                         
057400        MOVE MID-IDORDNSS-NEXT TO W-WDJ2-IDORDNSS                         
057500     ELSE                                                                 
057600        MOVE ZERO              TO W-WDJ2-IDORDNSS                         
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000 E-SAMMA-SIDA SECTION.                                                    
058100                                                                          
058200     PERFORM EA-SKAPA-NYCKEL-ENTER                                        
058300                                                                          
058400     PERFORM S03-SKAPA-INDEXNYCKEL                                        
058500                                                                          
058600     IF NOT ALLT-OK                                                       
058700        PERFORM MFS-RENSA-FAELT-UT                                        
058800        MOVE MAX-VARDE TO MOD-IDORDNSB-ENTER                              
058900                          MOD-IDORDNSS-ENTER                              
059000                          MOD-IDORDNSB-NEXT                               
059100                          MOD-IDORDNSS-NEXT                               
059200     END-IF                                                               
059300     .                                                                    
059400     EJECT                                                                
059500 EA-SKAPA-NYCKEL-ENTER SECTION.                                           
059600                                                                          
059700     IF MID-IDORDNSB-ENTER NUMERIC                                        
059800        MOVE MID-IDORDNSB-ENTER TO W-WDJ2-IDORDNSB                        
059900     ELSE                                                                 
060000        MOVE ZERO               TO W-WDJ2-IDORDNSB                        
060100     END-IF                                                               
060200                                                                          
060300     IF MID-IDORDNSS-ENTER NUMERIC                                        
060400        MOVE MID-IDORDNSS-ENTER TO W-WDJ2-IDORDNSS                        
060500     ELSE                                                                 
060600        MOVE ZERO               TO W-WDJ2-IDORDNSS                        
060700     END-IF                                                               
060800     .                                                                    
060900     EJECT                                                                
061000 F-LAES-VISA-INFO SECTION.                                                
061100                                                                          
061200     MOVE SHUV-IDORDNSB TO MOD-IDORDNSB-ENTER                             
061300     MOVE SHUV-IDORDNSS TO MOD-IDORDNSS-ENTER                             
061400                                                                          
061500     MOVE 1 TO INDX                                                       
061600                                                                          
061700     PERFORM UNTIL INDX > MAX-INDX                                        
061800        IF SEGMENT-FINNS                                                  
061900           PERFORM FA-FLYTTA-TILL-BILD                                    
062000           PERFORM IMS-GN-WDJ2-WLSATG01                                   
062100        ELSE                                                              
062200           PERFORM MFS-RENSA-FAELT-RAD-UT                                 
062300        END-IF                                                            
062400        ADD 1 TO INDX                                                     
062500     END-PERFORM                                                          
062600                                                                          
062700     IF SEGMENT-FINNS                                                     
062800        MOVE SHUV-IDORDNSB        TO MOD-IDORDNSB-NEXT                    
062900        MOVE SHUV-IDORDNSS        TO MOD-IDORDNSS-NEXT                    
063000        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
063100        PERFORM S02-INFO-MEDDELANDE                                       
063200     ELSE                                                                 
063300        MOVE MAX-VARDE            TO MOD-IDORDNSB-NEXT                    
063400                                     MOD-IDORDNSS-NEXT                    
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 FA-FLYTTA-TILL-BILD SECTION.                                             
063900                                                                          
064000     MOVE SPACE               TO MOD-KDCMD         (INDX)                 
064100     MOVE SHUV-IDARTNR        TO MOD-IDARTNR       (INDX)                 
064200     MOVE SHUV-IDORDNSB       TO MOD-IDORDNSB      (INDX)                 
064300     MOVE SHUV-IDORDNSS       TO MOD-IDORDNSS      (INDX)                 
064400     MOVE SHUV-SUSATPTI       TO MOD-SUSATPTI      (INDX)                 
064500     MOVE SHUV-DAREGDAT (3:6) TO MOD-TIREGDAT      (INDX)                 
064600     MOVE SHUV-KVBEART        TO MOD-KVBEART       (INDX)                 
064700     COMPUTE WS-KVRORAD = 99999 - SHUV-KVRORAD-9KOMPL                     
064800     MOVE WS-KVRORAD          TO MOD-KVRORAD       (INDX)                 
064900     MOVE SHUV-BEFT           TO MOD-BEFT          (INDX)                 
065000     MOVE SHUV-IDANSK         TO MOD-IDANSK        (INDX)                 
065100     PERFORM FAA-HAMTA-RESTSALDO                                          
065200     PERFORM FAB-SKAPA-ACK-PTID                                           
065300     .                                                                    
065400     EJECT                                                                
065500 FAA-HAMTA-RESTSALDO SECTION.                                             
065600     MOVE ZERO          TO WS-KVROS                                       
065700     MOVE SHUV-IDARTNR  TO W-IDARTNR                                      
065800     PERFORM IMS-GHU-WDK6-WLARTC11                                        
065900     IF SEGMENT-FINNS                                                     
066000        MOVE CLAG-KVROS TO WS-KVROS                                       
066100     END-IF                                                               
066200                                                                          
066300     MOVE WS-KVROS TO MOD-KVROS (INDX)                                    
066400     .                                                                    
066500     EJECT                                                                
066600 FAB-SKAPA-ACK-PTID SECTION.                                              
066700                                                                          
066800     MOVE SHUV-SUSATPTI TO WS-TIHHMM                                      
066900     ADD  WS-TIHH       TO WS-TIHH-ACK                                    
067000     ADD  WS-TIMM       TO WS-TIMM-ACK                                    
067100     IF WS-TIMM-ACK > 59                                                  
067200        DIVIDE WS-TIMM-ACK BY 60 GIVING WS-KVOT-TIHH                      
067300        COMPUTE WS-REST-TIMM = WS-TIMM-ACK - (WS-KVOT-TIHH * 60)          
067400        ADD  WS-KVOT-TIHH TO WS-TIHH-ACK                                  
067500        MOVE WS-REST-TIMM TO WS-TIMM-ACK                                  
067600     END-IF                                                               
067700     MOVE WS-TIHH-ACK   TO WS-TIHH                                        
067800     MOVE WS-TIMM-ACK   TO WS-TIMM                                        
067900                                                                          
068000     IF MFS-FIRST OR MFS-NEXT                                             
068100        MOVE WS-TIHHMM         TO MOD-SUACKPTI-NEXT                       
068200        MOVE MOD-SUACKPTI-NEXT TO MOD-SUACKPTI (INDX)                     
068300        IF INDX = 1                                                       
068400           MOVE MOD-SUACKPTI-NEXT TO MOD-SUACKPTI-ENTER                   
068500        END-IF                                                            
068600     ELSE                                                                 
068700        IF MFS-ENTER                                                      
068800           IF INDX = 1                                                    
068900              MOVE MOD-SUACKPTI-ENTER TO MOD-SUACKPTI (INDX)              
069000              MOVE MOD-SUACKPTI-ENTER TO MOD-SUACKPTI-NEXT                
069100           ELSE                                                           
069200              MOVE WS-TIHHMM          TO MOD-SUACKPTI-NEXT                
069300              MOVE MOD-SUACKPTI-NEXT  TO MOD-SUACKPTI (INDX)              
069400           END-IF                                                         
069500        END-IF                                                            
069600     END-IF                                                               
069700     .                                                                    
069800     EJECT                                                                
069900 G-KOLLA-INPUT SECTION.                                                   
070000                                                                          
070100     MOVE 1 TO INDX                                                       
070200     PERFORM UNTIL INDX > MAX-INDX                                        
070300        IF MID-KDCMD (INDX) = SPACE OR 'X'                                
070400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)             
070500        ELSE                                                              
070600           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)             
070700           MOVE NEJ                  TO INDATA-SW                         
070800        END-IF                                                            
070900        ADD 1 TO INDX                                                     
071000     END-PERFORM                                                          
071100                                                                          
071200     IF INDATA-OK                                                         
071300       MOVE 1 TO INDX                                                     
071400                 X-INDX                                                   
071500       PERFORM UNTIL INDX > MAX-INDX                                      
071600        IF MID-KDCMD (INDX) = SPACE                                       
071700          CONTINUE                                                        
071800        ELSE                                                              
071900          IF MID-KDCMD (INDX) = 'X' AND X-INDX = 1                        
072000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)           
072100             ADD 1 TO X-INDX                                              
072200          ELSE                                                            
072300             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)           
072400             MOVE NEJ                  TO INDATA-SW                       
072500          END-IF                                                          
072600        END-IF                                                            
072700        ADD 1 TO INDX                                                     
072800       END-PERFORM                                                        
072900     END-IF                                                               
073000                                                                          
073100     IF INDATA-FEL                                                        
073200        MOVE ERR-CORR-FIELDS TO MED-IDMFSFEL                              
073300        PERFORM S01-FEL-MEDDELANDE                                        
073400        PERFORM MFS-ROER-EJ-FAELT-UT                                      
073500        MOVE NEJ TO ALLT-SW                                               
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 H-UPPDATERA SECTION.                                                     
074000                                                                          
074100     MOVE NEJ TO ALLT-SW                                                  
074200     MOVE 1 TO INDX                                                       
074300     PERFORM UNTIL INDX > MAX-INDX                                        
074400        IF MID-KDCMD (INDX) = 'X'                                         
074500           IF MID-IDORDNSB (INDX) NUMERIC                                 
074600              AND                                                         
074700              MID-IDORDNSS (INDX) NUMERIC                                 
074800              MOVE MID-IDORDNSB (INDX) TO W-WDJ2-IDORDNSB                 
074900              MOVE MID-IDORDNSS (INDX) TO W-WDJ2-IDORDNSS                 
075000              PERFORM S04-KOLLA-BYGGBAR                                   
075100           END-IF                                                         
075200           MOVE 99 TO INDX                                                
075300        END-IF                                                            
075400        ADD 1 TO INDX                                                     
075500     END-PERFORM                                                          
075600                                                                          
075700     IF SATS-BYGGBAR                                                      
075800        PERFORM MFS-RENSA-FAELT-UT                                        
075900     ELSE                                                                 
076000        MOVE INF-KIT-NOT-FILLED TO MED-IDMFSINF                           
076100        PERFORM S02-INFO-MEDDELANDE                                       
076200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
076300     END-IF                                                               
076400     .                                                                    
076500     EJECT                                                                
076600 I-SKICKA-IMSTRANS SECTION.                                               
076700                                                                          
076800     IF (MFS-UPDATE OR MFS-PRINT)                                         
076900        AND                                                               
077000        SATS-BYGGBAR                                                      
077100        PERFORM IA-SKAPA-ALT-MOD                                          
077200        PERFORM IMS-INSERT-MSG-ALT                                        
077300     ELSE                                                                 
077400        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
077500        PERFORM IMS-INSERT-MSG                                            
077600     END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900 IA-SKAPA-ALT-MOD SECTION.                                                
078000                                                                          
078100     MOVE MIN-MOD-LAENGD       TO MSG-KVLL                                
078200     MOVE 'W4T371X '           TO MSG-KDTRANS-1                           
078300     MOVE '4354'               TO MSG-IDTRANS-1                           
078400     MOVE 1                    TO MSG-KDMFSFOR-1                          
078500     MOVE MID-W4I35401         TO MOD-MID-W4I37101                        
078600     MOVE MOD-IDORDNSB-BYGGBAR TO MOD-MID-IDORDNSB-BYGGBAR                
078700     MOVE MOD-IDORDNSS-BYGGBAR TO MOD-MID-IDORDNSS-BYGGBAR                
078800     MOVE MOD-MID-W4I37101     TO MSG-INDATA-MINUS-1-TRANSKOD             
078900     .                                                                    
079000     EJECT                                                                
079100 J-KOLLA-PRINT SECTION.                                                   
079200                                                                          
079300     MOVE 2 TO INDX                                                       
079400     PERFORM UNTIL INDX > MAX-INDX                                        
079500       IF MID-KDCMD (INDX) = 'X'                                          
079600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
079700         PERFORM S02-INFO-MEDDELANDE                                      
079800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
079900         PERFORM MFS-LAES-IN-IGEN                                         
080000         MOVE NEJ TO ALLT-SW                                              
080100         MOVE 99 TO INDX                                                  
080200       END-IF                                                             
080300       ADD 1 TO INDX                                                      
080400     END-PERFORM                                                          
080500     IF INDX < 99                                                         
080600       MOVE MID-IDORDNSB-BYGGBAR TO W-WDJ2-IDORDNSB                       
080700       MOVE MID-IDORDNSS-BYGGBAR TO W-WDJ2-IDORDNSS                       
080800                                                                          
080900       PERFORM S04-KOLLA-BYGGBAR                                          
081000                                                                          
081100       IF SATS-BYGGBAR                                                    
081200          PERFORM MFS-RENSA-FAELT-UT                                      
081300       ELSE                                                               
081400          MOVE INF-KIT-NOT-FILLED TO MED-IDMFSINF                         
081500          PERFORM S02-INFO-MEDDELANDE                                     
081600          PERFORM MFS-ROER-EJ-FAELT-UT                                    
081700       END-IF                                                             
081800     END-IF                                                               
081900     .                                                                    
082000     EJECT                                                                
082100 S01-FEL-MEDDELANDE SECTION.                                              
082200                                                                          
082300     CALL WMEDKONV USING MED-WMEDAREA                                     
082400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
082500     .                                                                    
082600                                                                          
082700                                                                          
082800 S02-INFO-MEDDELANDE SECTION.                                             
082900                                                                          
083000     CALL WMEDKONV USING MED-WMEDAREA                                     
083100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
083200     .                                                                    
083300     EJECT                                                                
083400 S03-SKAPA-INDEXNYCKEL SECTION.                                           
083500                                                                          
083600     PERFORM IMS-GU-WDJ2-WLSATG01                                         
083700                                                                          
083800     IF SEGMENT-FINNS                                                     
083900        MOVE +1              TO W-WDJ2ASEQ-MIN-KDCLAGER                   
084000                                W-WDJ2ASEQ-MAX-KDCLAGER                   
084100        MOVE SHUV-IDPRC      TO W-WDJ2ASEQ-MIN-IDPRC                      
084200                                W-WDJ2ASEQ-MAX-IDPRC                      
084300        MOVE SHUV-KVRORAD-9KOMPL                                          
084400                             TO W-WDJ2ASEQ-MIN-KVRORAD-9KOMPL             
084500        MOVE SHUV-RELSKVOT-L TO W-WDJ2ASEQ-MIN-RELSKVOT-L                 
084600        MOVE SHUV-FLSATPRI   TO W-WDJ2ASEQ-MIN-FLSATPRI                   
084700        MOVE SHUV-RELSKVOT-S TO W-WDJ2ASEQ-MIN-RELSKVOT-S                 
084800        MOVE SHUV-IDARTNR    TO W-WDJ2ASEQ-MIN-IDARTNR                    
084900        MOVE SHUV-DAREGDAT   TO W-WDJ2ASEQ-MIN-DAREGDAT                   
085000        PERFORM IMS-GU-WDJ2-SEQ-WLSATG01                                  
085100        MOVE NEJ TO WS-TRAEFF                                             
085200        PERFORM UNTIL TRAEFF                                              
085300           IF SEGMENT-SAKNAS OR END-OF-DATA                               
085400              MOVE NEJ TO ALLT-SW                                         
085500              MOVE  JA TO WS-TRAEFF                                       
085600           ELSE                                                           
085700              IF SHUV-IDORDNSB = W-WDJ2-IDORDNSB                          
085800                 AND                                                      
085900                 SHUV-IDORDNSS = W-WDJ2-IDORDNSS                          
086000                 MOVE JA TO WS-TRAEFF                                     
086100              ELSE                                                        
086200                 PERFORM IMS-GN-WDJ2-WLSATG01                             
086300              END-IF                                                      
086400           END-IF                                                         
086500        END-PERFORM                                                       
086600     ELSE                                                                 
086700        MOVE NEJ TO ALLT-SW                                               
086800     END-IF                                                               
086900     .                                                                    
087000     EJECT                                                                
087100 S04-KOLLA-BYGGBAR SECTION.                                               
087200                                                                          
087300     PERFORM IMS-GU-WDJ2-WLSATG01                                         
087400                                                                          
087500     IF SEGMENT-FINNS                                                     
087600        IF SHUV-FLBYGGB = NEJ                                             
087700           MOVE NEJ TO BYGGBAR-SW                                         
087800        ELSE                                                              
087900           IF SHUV-KDSATSTA = 'R'                                         
088000              MOVE SHUV-KVBYGGB  TO WS-KVBYGGB-SPAR                       
088100              PERFORM S05-KOLLA-ING-ARTIKLAR                              
088200              PERFORM IMS-GHU-WDJ2-WLSATG01                               
088300              IF SATS-EJ-BYGGBAR                                          
088400                 MOVE NEJ             TO SHUV-FLBYGGB                     
088500                 MOVE WS-KVBYGGB-SPAR TO SHUV-KVBYGGB                     
088600                 MOVE ZERO            TO MOD-IDORDNSB-BYGGBAR             
088700                 MOVE ZERO            TO MOD-IDORDNSS-BYGGBAR             
088800              ELSE                                                        
088900                 IF SHUV-IDPRODNR = 0                                     
089000                    PERFORM S04A-CALL-W411ORDN                            
089100                 END-IF                                                   
089200                 MOVE '1'             TO SHUV-KDSATPLK                    
089300                 MOVE SHUV-IDORDNSB   TO MOD-IDORDNSB-BYGGBAR             
089400                 MOVE SHUV-IDORDNSS   TO MOD-IDORDNSS-BYGGBAR             
089500                 IF MFS-UPDATE                                            
089600                    MOVE WS-IDUSER    TO SHUV-IDUSER                      
089700                 END-IF                                                   
089800              END-IF                                                      
089900           PERFORM IMS-REPL-WDJ2-WLSATG01                                 
090000           END-IF                                                         
090100        END-IF                                                            
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 S04A-CALL-W411ORDN  SECTION.                                             
090600                                                                          
090700     MOVE 'UTSK'           TO ORDN-IDSYSTEM                               
090800     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB                      
090900                                       ORDN-ORQL-PCB                      
091000                                       ORDN-PROC-PCB                      
091100                                       ORDN-ORQI-PCB                      
091200                                       ORDN-WDQ3-PCB                      
091300     MOVE ORDN-IDPRODNR-UT TO SHUV-IDPRODNR                               
091400     .                                                                    
091500     EJECT                                                                
091600 S05-KOLLA-ING-ARTIKLAR SECTION.                                          
091700                                                                          
091800     PERFORM IMS-GHNP-WDJ2-WLSATG11                                       
091900                                                                          
092000     IF SEGMENT-FINNS                                                     
092100        MOVE JA TO BYGGBAR-SW                                             
092200     END-IF                                                               
092300     PERFORM UNTIL SEGMENT-SAKNAS                                         
092400        IF SRAD-KVSATRES      >  0                                        
092500           MOVE SRAD-IDARTNR  TO W-IDARTNR                                
092600           PERFORM IMS-GHU-WDK6-WLARTC11                                  
092700           PERFORM S06-KOLLA-CLAGER-SPAERR                                
092800           IF CLAGER-SPAERR                                               
092900              PERFORM S07-CLAGER-SPAERRAT                                 
093000              PERFORM S10-SKAPA-RESTORDER                                 
093100              MOVE ZERO TO WS-KVBYGGB-SPAR                                
093200              MOVE NEJ  TO BYGGBAR-SW                                     
093300           ELSE                                                           
093400              PERFORM S08-KOLLA-DISP-SALDO                                
093500           END-IF                                                         
093600        END-IF                                                            
093700        PERFORM IMS-GHNP-WDJ2-WLSATG11                                    
093800     END-PERFORM                                                          
093900     .                                                                    
094000     EJECT                                                                
094100 S06-KOLLA-CLAGER-SPAERR SECTION.                                         
094200                                                                          
094300     MOVE NEJ            TO CLAGER-SW                                     
094400     IF CLAG-KDLEVSP     =  20 OR 21                                      
094500         MOVE JA         TO CLAGER-SW                                     
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 S07-CLAGER-SPAERRAT SECTION.                                             
095000                                                                          
095100        PERFORM S13-EV-LARM-2191-ANSKAFFN                                 
095200                                                                          
095300        SUBTRACT SRAD-KVSATRES FROM CLAG-KVRESS                           
095400        ADD      SRAD-KVSATRES   TO CLAG-KVROS                            
095500        MOVE     SRAD-KVSATRES   TO SRAD-KVSATROS                         
095600                                    WS-KVART                              
095700                                    WS-KVRO                               
095800        MOVE ZERO                TO SRAD-KVSATRES                         
095900                                    SRAD-VKARTNTO                         
096000                                    SRAD-VLARTNTO                         
096100                                                                          
096200        PERFORM IMS-REPL-WDJ2-WLSATG11                                    
096300                                                                          
096400        PERFORM IMS-REPL-WDD6-WLARTC11                                    
096500     .                                                                    
096600     EJECT                                                                
096700 S08-KOLLA-DISP-SALDO SECTION.                                            
096800                                                                          
096900     IF CLAG-KVUTRS > 0                                                   
097000        COMPUTE WS-DISPSALDO = (CLAG-KVLS -                               
097100                                CLAG-KVSPANT - CLAG-KVUTRS)               
097200        IF WS-DISPSALDO < CLAG-KVUTRS                                     
097300           AND                                                            
097400           WS-DISPSALDO < SRAD-KVSATRES                                   
097500           PERFORM S09-UPPDAT-ARTREG-SATSRAD                              
097600           PERFORM S10-SKAPA-RESTORDER                                    
097700           MOVE NEJ TO BYGGBAR-SW                                         
097800        END-IF                                                            
097900     ELSE                                                                 
098000        COMPUTE WS-DISPSALDO = (CLAG-KVLS - CLAG-KVSPANT)                 
098100        IF WS-DISPSALDO < SRAD-KVSATRES                                   
098200           PERFORM S09-UPPDAT-ARTREG-SATSRAD                              
098300           PERFORM S10-SKAPA-RESTORDER                                    
098400           MOVE NEJ TO BYGGBAR-SW                                         
098500        END-IF                                                            
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 S09-UPPDAT-ARTREG-SATSRAD SECTION.                                       
099000                                                                          
099100     IF WS-DISPSALDO NEGATIVE                                             
099200        MOVE SRAD-KVSATRES TO WS-KVRO WS-KVART                            
099300        MOVE ZERO          TO WS-KVSATRES                                 
099400     ELSE                                                                 
099500        COMPUTE WS-KVRO = SRAD-KVSATRES - WS-DISPSALDO                    
099600        MOVE WS-KVRO       TO WS-KVART                                    
099700        MOVE WS-DISPSALDO  TO WS-KVSATRES                                 
099800     END-IF                                                               
099900                                                                          
100000     PERFORM S13-EV-LARM-2191-ANSKAFFN                                    
100100     ADD WS-KVRO      TO   CLAG-KVROS                                     
100200     SUBTRACT WS-KVRO FROM CLAG-KVRESS                                    
100300     PERFORM IMS-REPL-WDD6-WLARTC11                                       
100400                                                                          
100500     SUBTRACT WS-KVRO FROM SRAD-KVSATRES                                  
100600     ADD      WS-KVRO TO   SRAD-KVSATROS                                  
100700     PERFORM IMS-REPL-WDJ2-WLSATG11                                       
100800                                                                          
100900     COMPUTE WS-KVBYGGB = WS-KVSATRES / SRAD-REANTPSA                     
101000             ON SIZE ERROR MOVE 0 TO WS-KVBYGGB                           
101100     END-COMPUTE                                                          
101200                                                                          
101300     IF SRAD-KDSATKMB NOT = SPACE                                         
101400        PERFORM S09A-BEHANDLA-KOMB-KVBYGGBAR                              
101500     END-IF                                                               
101600                                                                          
101700     IF WS-KVBYGGB < WS-KVBYGGB-SPAR                                      
101800        MOVE WS-KVBYGGB TO WS-KVBYGGB-SPAR                                
101900     END-IF                                                               
102000     .                                                                    
102100     EJECT                                                                
102200 S09A-BEHANDLA-KOMB-KVBYGGBAR SECTION.                                    
102300                                                                          
102400     MOVE SRAD-IDARTNR  TO WS-SPAR-IDARTNR                                
102500     MOVE SRAD-KDSATKMB TO WS-SPAR-KDSATKMB                               
102600                                                                          
102700     PERFORM IMS-GNP-WDJ2-WLSATG11-FIRST                                  
102800                                                                          
102900     PERFORM UNTIL SEGMENT-SAKNAS                                         
103000        IF SRAD-IDARTNR NOT = WS-SPAR-IDARTNR                             
103100           IF SRAD-KDSATKMB = WS-SPAR-KDSATKMB                            
103200              COMPUTE WS-KVBYGGB-SATKMB = SRAD-KVSATRES /                 
103300                                          SRAD-REANTPSA                   
103400                      ON SIZE ERROR MOVE 0 TO WS-KVBYGGB-SATKMB           
103500              END-COMPUTE                                                 
103600              COMPUTE WS-KVBYGGB = WS-KVBYGGB + WS-KVBYGGB-SATKMB         
103700           END-IF                                                         
103800        END-IF                                                            
103900        PERFORM IMS-GNP-WDJ2-WLSATG11                                     
104000     END-PERFORM                                                          
104100                                                                          
104200     MOVE WS-SPAR-IDARTNR TO W-WDJ2-IDARTNR                               
104300     PERFORM IMS-GU-WDJ2-WLSATG11                                         
104400     .                                                                    
104500     EJECT                                                                
104600 S10-SKAPA-RESTORDER SECTION.                                             
104700                                                                          
104800     MOVE SHUV-IDDISTR            TO RO-RAD-IDDISTR                       
104900     MOVE SHUV-IDKUNDNR           TO RO-RAD-IDKUNDNR                      
105000     MOVE SPACE                   TO RO-RAD-IDKUNDRF                      
105100     MOVE SHUV-IDORDNSB           TO RO-RAD-IDORDNR5                      
105200     MOVE RO-RAD-IDKUNDRF (2:1)   TO RO-RAD-IDKUNDRF (1:1)                
105300     MOVE RO-RAD-IDKUNDRF (3:1)   TO RO-RAD-IDKUNDRF (2:1)                
105400     MOVE RO-RAD-IDKUNDRF (4:1)   TO RO-RAD-IDKUNDRF (3:1)                
105500     MOVE RO-RAD-IDKUNDRF (5:1)   TO RO-RAD-IDKUNDRF (4:1)                
105600     MOVE SHUV-IDORDNSS           TO RO-RAD-IDKUNDRF (5:1)                
105700     MOVE SRAD-IDARTNR            TO RO-RAD-IDARTNR                       
105800     MOVE WC-CDC-SE               TO RO-RAD-IDDC                          
105900                                     RO-RAD-IDDC-RO                       
106000     MOVE 'KI'                    TO RO-RAD-KDOI                          
106100     MOVE SPACE                   TO RO-RAD-CLEARGROUP                    
106200     MOVE 1                       TO RO-RAD-IDLOPNR                       
106300     MOVE SPACE                   TO RO-RAD-BEKUNDRF                      
106400     MOVE SPACE                   TO RO-RAD-BERADREF                      
106500     MOVE NEJ                     TO RO-RAD-FLERS                         
106600     MOVE SHUV-IDKONTO            TO RO-RAD-IDKONTO                       
106700     MOVE SHUV-IDKST              TO RO-RAD-IDKST                         
106800     MOVE SHUV-IDANALYS           TO RO-RAD-IDANALYS                      
106900     MOVE SPACE                   TO RO-RAD-IDKUNDRF-LEV                  
107000     MOVE ZERO                    TO RO-RAD-KDDSP                         
107100     MOVE SHUV-KDFAKTYP           TO RO-RAD-KDFAKTYP                      
107200     MOVE ZERO                    TO RO-RAD-KDFRAKT                       
107300     MOVE ZERO                    TO RO-RAD-KDKVBRYT                      
107400     MOVE 1                       TO RO-RAD-KDORDING                      
107500     MOVE SHUV-KDORDKL            TO RO-RAD-KDORDKL                       
107600     MOVE SRAD-KDPRODSL           TO RO-RAD-KDPRODSL                      
107700     MOVE WS-KDROO-1              TO RO-RAD-KDROO                         
107800     MOVE WS-KVRO                 TO RO-RAD-KVRO                          
107900     MOVE '2'                     TO RO-RAD-KDSTARAD                      
108000     MOVE ZERO                    TO RO-RAD-KDTPOTYP                      
108100     MOVE ZERO                    TO RO-RAD-KDVRINFO                      
108200     MOVE WS-KVART                TO RO-RAD-KVART                         
108300     INITIALIZE                      RO-RAD-DEAL-PR-LINE                  
108400     MOVE ZERO                    TO RO-RAD-PRARTNTO                      
108500     MOVE SRAD-REKSIFFR           TO RO-RAD-REKSIFFR                      
108600     MOVE ZERO                    TO RO-RAD-TIAVBOKN                      
108700     MOVE SHUV-DAREGDAT (3:6)     TO RO-RAD-TIREGDAT                      
108800     MOVE ZERO                    TO RO-RAD-TIRES                         
108900     MOVE WS-DATUM                TO RO-RAD-DARODAT                       
109000     MOVE ZERO                    TO RO-RAD-TITPO                         
109100     MOVE SPACE                   TO RO-RAD-KDPRTYP                       
109200     MOVE SPACE                   TO RO-RAD-BEVOLREF                      
109300     MOVE NEJ                     TO RO-RAD-FLINVEST                      
109400     MOVE NEJ                     TO RO-RAD-FLPRTILL                      
109500     MOVE JA                      TO RO-RAD-FLTPOBEK                      
109600     MOVE ZERO                    TO RO-RAD-IDKAMPRF                      
109700     MOVE SPACE                   TO RO-RAD-IDLEVNR                       
109800     MOVE 'SATS'                  TO RO-RAD-IDSYSTEM                      
109900     MOVE SRAD-REBEART            TO RO-RAD-KVBEART-Q                     
110000     MOVE WS-TIME (1:6)           TO RO-RAD-TIREGTID                      
110100     MOVE 999                     TO RO-RAD-DASENDAT                      
110200                                     RO-RAD-TISENBEK-KL                   
110300     MOVE CLAG-IDANSK             TO RO-RAD-IDANSK                        
110400                                                                          
110500     MOVE SPACE                   TO RO-RAD-KDORDTYP-LDC                  
110600     MOVE ZERO                    TO RO-RAD-TIREPDAT                      
110700     MOVE SPACE                   TO RO-RAD-IDKUNDRF-WIP                  
110810     MOVE +0                      TO RO-RAD-PRAVCOST                      
110820     MOVE SPACE                   TO RO-RAD-KDROPACK                      
110830     MOVE SPACE                   TO RO-RAD-IDARBREF                      
110900                                                                          
111000     PERFORM S11-HAMTA-PRIORITETSKOD                                      
111100                                                                          
111200     PERFORM IMS-ISRT-WDA5-WLORDP01                                       
111300                                                                          
111400     PERFORM UNTIL SEGMENT-FINNS                                          
111500        ADD 1 TO RO-RAD-IDLOPNR                                           
111600        PERFORM IMS-ISRT-WDA5-WLORDP01                                    
111700     END-PERFORM                                                          
111800     .                                                                    
111900     EJECT                                                                
112000 S11-HAMTA-PRIORITETSKOD SECTION.                                         
112100                                                                          
112200     MOVE RO-RAD-KDTPOTYP TO W-4512-KDTPOTYP                              
112300     MOVE SHUV-KDORDKL    TO W-4512-KDORDKL                               
112400     MOVE SHUV-IDDISTR    TO W-4512-IDDISTR-FOM                           
112500                             W-4512-IDDISTR-TOM                           
112600                                                                          
112700     PERFORM IMS-GU-XXJN-WLXXJN11                                         
112800     MOVE 4512-KDRAPRIO  TO RO-RAD-KDRAPRIO                               
112900     .                                                                    
113000     EJECT                                                                
113100 S13-EV-LARM-2191-ANSKAFFN SECTION.                                       
113200                                                                          
113300** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS.             
113400     IF CLAG-KVROS = 0                                                    
113500        IF CLAG-KVAKS-CDC + CLAG-KVAKS-PAV = 0                            
113600          IF ANTAL-ISRT-2191 < 23                                         
113700                                                                          
113800            COMPUTE ALT2191-LL =                                          
113801                    LENGTH OF ALT2191-MID-W2I19101 + 17                   
113802            ADD +1              TO ANTAL-ISRT-2191                        
113900            MOVE '1'            TO ALT2191-MID-KDCLAGER                   
114000            MOVE SRAD-IDARTNR   TO ALT2191-MID-IDARTNR                    
114100            MOVE ZERO           TO ALT2191-MID-TISENBEK-DAG               
114200                                    ALT2191-MID-TISENBEK-KL               
114300            MOVE SPACE          TO ALT2191-MID-IDKR                       
114400            MOVE CLAG-IDANSK    TO ALT2191-MID-IDANSK                     
114500            MOVE 210            TO ALT2191-MID-KDLARM                     
114600            MOVE SHUV-IDDISTR   TO WS-IDDISTR-NUM4                        
114700            MOVE WS-IDDISTR-NUM4 TO ALT2191-MID-IDDISTR                   
114800            MOVE SHUV-IDKUNDNR TO WS-IDKUNDNR-NUM6                        
114900            MOVE WS-IDKUNDNR-NUM6 TO ALT2191-MID-IDKUNDNR                 
115000            MOVE SHUV-IDORDNSB TO ALT2191-MID-IDORDNR5(1:4)               
115100            MOVE SHUV-IDORDNSS TO ALT2191-MID-IDKUNDRF(5:1)               
115200            MOVE 'J'            TO ALT2191-MID-FLNYLARM                   
115210            MOVE WC-CDC-SE      TO ALT2191-MID-IDDC                       
115220            MOVE SPACE          TO ALT2191-MID-IDLEVNR                    
115300                                                                          
115400            PERFORM IMS-PURG-ALT2191-MSG                                  
115500          END-IF                                                          
115600        END-IF                                                            
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000 MFS-RENSA-FAELT-UT SECTION.                                              
116100                                                                          
116200     MOVE MFS-RENSA-FAELT TO MOD-IDORDNSB-ENTER                           
116300                             MOD-IDORDNSS-ENTER                           
116400                             MOD-IDORDNSB-NEXT                            
116500                             MOD-IDORDNSS-NEXT                            
116600     MOVE 1 TO INDX                                                       
116700     PERFORM UNTIL INDX > MAX-INDX                                        
116800        PERFORM MFS-RENSA-FAELT-RAD-UT                                    
116900        ADD 1 TO INDX                                                     
117000     END-PERFORM                                                          
117100     .                                                                    
117200     EJECT                                                                
117300 MFS-RENSA-FAELT-RAD-UT SECTION.                                          
117400                                                                          
117500     MOVE MFS-RENSA-FAELT TO MOD-KDCMD        (INDX)                      
117600                             MOD-IDARTNR      (INDX)                      
117700                             MOD-IDORDNSB     (INDX)                      
117800                             MOD-IDORDNSS     (INDX)                      
117900                             MOD-SUSATPTI     (INDX)                      
118000                             MOD-SUACKPTI     (INDX)                      
118100                             MOD-TIREGDAT     (INDX)                      
118200                             MOD-KVBEART      (INDX)                      
118300                             MOD-KVRORAD      (INDX)                      
118400                             MOD-KVROS        (INDX)                      
118500                             MOD-BEFT         (INDX)                      
118600                             MOD-IDANSK       (INDX)                      
118700     .                                                                    
118800     EJECT                                                                
118900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
119000                                                                          
119100                                                                          
119200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDORDNSB-ENTER                         
119300                               MOD-IDORDNSS-ENTER                         
119400                               MOD-IDORDNSB-NEXT                          
119500                               MOD-IDORDNSS-NEXT                          
119600     MOVE 1 TO INDX                                                       
119700     PERFORM UNTIL INDX > MAX-INDX                                        
119800        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD        (INDX)                 
119900                                  MOD-IDARTNR      (INDX)                 
120000                                  MOD-IDORDNSB     (INDX)                 
120100                                  MOD-IDORDNSS     (INDX)                 
120200                                  MOD-SUSATPTI     (INDX)                 
120300                                  MOD-SUACKPTI     (INDX)                 
120400                                  MOD-TIREGDAT     (INDX)                 
120500                                  MOD-KVBEART      (INDX)                 
120600                                  MOD-KVRORAD      (INDX)                 
120700                                  MOD-KVROS        (INDX)                 
120800                                  MOD-BEFT         (INDX)                 
120900                                  MOD-IDANSK       (INDX)                 
121000        ADD 1 TO INDX                                                     
121100     END-PERFORM                                                          
121200     .                                                                    
121300     EJECT                                                                
121400 MFS-LAES-IN-IGEN SECTION.                                                
121500                                                                          
121600     MOVE 1 TO INDX                                                       
121700     PERFORM UNTIL INDX > MAX-INDX                                        
121800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)               
121900        ADD 1 TO INDX                                                     
122000     END-PERFORM                                                          
122100     .                                                                    
122200     EJECT                                                                
122300* --- IMS SEKTIONER ---                                                   
122400     SKIP3                                                                
122500 IMS-GET-MSG SECTION.                                                     
122600                                                                          
122700     MOVE '  QC' TO GODK-STATUSKODER                                      
122800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
122900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123000     PERFORM IMS-STATUSKONTROLL                                           
123100     .                                                                    
123200     SKIP3                                                                
123300 IMS-INSERT-MSG SECTION.                                                  
123400                                                                          
123500     IF NOT ENGLISH-TEXT                                                  
123600       MOVE '0' TO MFS-KDHUVOMR                                           
123700     END-IF                                                               
123800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
123900     MOVE SPACE TO GODK-STATUSKODER                                       
124000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
124100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     EJECT                                                                
124500 IMS-INSERT-MSG-ALT SECTION.                                              
124600                                                                          
124700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
124800     MOVE SPACE TO GODK-STATUSKODER                                       
124900     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
125000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
125100     PERFORM IMS-STATUSKONTROLL                                           
125200     .                                                                    
125300     EJECT                                                                
125400 IMS-PURG-ALT2191-MSG SECTION.                                            
125500     MOVE LOW-VALUE TO ALT2191-Z1 ALT2191-Z2                              
125600     MOVE '  '  TO GODK-STATUSKODER                                       
125700     CALL CBLTDLI USING PURG ALT2191-PCB ALT2191-IO-AREA                  
125800     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     .                                                                    
126100     SKIP2                                                                
126200 IMS-GN-WDJ2-WLSATG01 SECTION.                                            
126300                                                                          
126400     STRING 'WLSATG01(WDJ2ASEQ>=' W-WDJ2ASEQ-MIN-X                        
126500                    '&WDJ2ASEQ<=' W-WDJ2ASEQ-MAX-X ')'                    
126600          DELIMITED BY SIZE INTO SSA1                                     
126700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
126800     CALL CBLTDLI USING GN SATH-PCB DLI-IO-AREA1 SSA1                     
126900     MOVE SATH-STATUS-CODE TO STATUS-WS                                   
127000     PERFORM IMS-STATUSKONTROLL                                           
127100     .                                                                    
127200                                                                          
127300                                                                          
127400 IMS-GU-WDJ2-SEQ-WLSATG01 SECTION.                                        
127500                                                                          
127600     STRING 'WLSATG01(WDJ2ASEQ =' W-WDJ2ASEQ-MIN-X ')'                    
127700          DELIMITED BY SIZE INTO SSA1                                     
127800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
127900     CALL CBLTDLI USING GU SATH-PCB DLI-IO-AREA1 SSA1                     
128000     MOVE SATH-STATUS-CODE TO STATUS-WS                                   
128100     PERFORM IMS-STATUSKONTROLL                                           
128200     .                                                                    
128300                                                                          
128400                                                                          
128500 IMS-GU-WDJ2-WLSATG01 SECTION.                                            
128600                                                                          
128700     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
128800          DELIMITED BY SIZE INTO SSA1                                     
128900     MOVE '  GE' TO GODK-STATUSKODER                                      
129000     CALL CBLTDLI USING GU SATG-PCB DLI-IO-AREA1 SSA1                     
129100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400                                                                          
129500                                                                          
129600 IMS-GHU-WDJ2-WLSATG01 SECTION.                                           
129700                                                                          
129800     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
129900          DELIMITED BY SIZE INTO SSA1                                     
130000     MOVE '    ' TO GODK-STATUSKODER                                      
130100     CALL CBLTDLI USING GHU SATG-PCB DLI-IO-AREA1 SSA1                    
130200     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
130300     PERFORM IMS-STATUSKONTROLL                                           
130400     .                                                                    
130500                                                                          
130600                                                                          
130700 IMS-REPL-WDJ2-WLSATG01 SECTION.                                          
130800                                                                          
130900     MOVE '    ' TO GODK-STATUSKODER                                      
131000     CALL CBLTDLI USING REPL SATG-PCB DLI-IO-AREA1                        
131100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
131200     PERFORM IMS-STATUSKONTROLL                                           
131300     .                                                                    
131400     EJECT                                                                
131500 IMS-GNP-WDJ2-WLSATG11-FIRST SECTION.                                     
131600                                                                          
131700     MOVE 'WLSATG11*F' TO SSA1                                            
131800     MOVE '  GE' TO GODK-STATUSKODER                                      
131900     CALL CBLTDLI USING GNP SATG-PCB DLI-IO-AREA2 SSA1                    
132000     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     .                                                                    
132300                                                                          
132400                                                                          
132500 IMS-GNP-WDJ2-WLSATG11 SECTION.                                           
132600                                                                          
132700     MOVE 'WLSATG11 ' TO SSA1                                             
132800     MOVE '  GE' TO GODK-STATUSKODER                                      
132900     CALL CBLTDLI USING GNP SATG-PCB DLI-IO-AREA2 SSA1                    
133000     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300                                                                          
133400                                                                          
133500 IMS-GU-WDJ2-WLSATG11 SECTION.                                            
133600                                                                          
133700     STRING 'WLSATG01*P(IDORDNST =' W-WDJ2-IDORDNST-X ')'                 
133800          DELIMITED BY SIZE INTO SSA1                                     
133900     STRING 'WLSATG11(IDARTNR  =' W-WDJ2-IDARTNR-X ')'                    
134000          DELIMITED BY SIZE INTO SSA2                                     
134100     MOVE '    ' TO GODK-STATUSKODER                                      
134200     CALL CBLTDLI USING GU SATG-PCB DLI-IO-AREA2 SSA1 SSA2                
134300     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
134400     PERFORM IMS-STATUSKONTROLL                                           
134500     .                                                                    
134600                                                                          
134700                                                                          
134800 IMS-GHNP-WDJ2-WLSATG11 SECTION.                                          
134900                                                                          
135000     MOVE 'WLSATG11 ' TO SSA1                                             
135100     MOVE '  GE' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING GHNP SATG-PCB DLI-IO-AREA2 SSA1                   
135300     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600                                                                          
135700                                                                          
135800 IMS-REPL-WDJ2-WLSATG11 SECTION.                                          
135900                                                                          
136000     MOVE '    ' TO GODK-STATUSKODER                                      
136100     CALL CBLTDLI USING REPL SATG-PCB DLI-IO-AREA2                        
136200     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     .                                                                    
136500     EJECT                                                                
136600                                                                          
136700 IMS-GHU-WDK6-WLARTC11 SECTION.                                           
136800                                                                          
136900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
137000          DELIMITED BY SIZE INTO SSA1                                     
137100     MOVE 'WLARTC11 ' TO SSA2                                             
137200     MOVE '    ' TO GODK-STATUSKODER                                      
137300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA3 SSA1 SSA2               
137400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
137500     PERFORM IMS-STATUSKONTROLL                                           
137600     .                                                                    
137700                                                                          
137800                                                                          
137900 IMS-REPL-WDD6-WLARTC11 SECTION.                                          
138000                                                                          
138100     MOVE '    ' TO GODK-STATUSKODER                                      
138200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA3                        
138300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
138400     PERFORM IMS-STATUSKONTROLL                                           
138500     .                                                                    
138600     EJECT                                                                
138700 IMS-ISRT-WDA5-WLORDP01 SECTION.                                          
138800                                                                          
138900     MOVE 'WLORDP01 ' TO SSA1                                             
139000     MOVE '  II' TO GODK-STATUSKODER                                      
139100     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA5 SSA1                   
139200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
139300     PERFORM IMS-STATUSKONTROLL                                           
139400     .                                                                    
139500     EJECT                                                                
139600 IMS-GU-XXJN-WLXXJN11 SECTION.                                            
139700                                                                          
139800     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
139900          DELIMITED BY SIZE INTO SSA1                                     
140000     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
140100                    '&KDORDKL  =' W-4512-KDORDKL-X                        
140200                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
140300                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
140400          DELIMITED BY SIZE INTO SSA2                                     
140500     MOVE '    ' TO GODK-STATUSKODER                                      
140600     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA4 SSA1 SSA2                
140700     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000     EJECT                                                                
141100 IMS-STATUSKONTROLL SECTION.                                              
141200                                                                          
141300     SET STATUS-IX TO 1                                                   
141400     SEARCH GODK-STATUS                                                   
141500       AT END                                                             
141600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
141700         DELIMITED BY SIZE INTO FELTEXT                                   
141800         CALL FELLOG                                                      
141900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
142000     END-SEARCH                                                           
142100     .                                                                    
