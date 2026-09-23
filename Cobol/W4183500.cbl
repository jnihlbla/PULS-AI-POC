000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4183500.                                                
000400*AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500*DATE-WRITTEN.   95/08/14.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        GÖR UPPDATERINGAR FÖR PROGRAM W4183000.                          
001100*        POSTTYPEN STYR VILKEN VAD SOM SKALL UPPDATERAS.                  
001200*        POSTTYP:                                                         
001300*        001 = TILLÄGGSFAKTURA UPPDATERAR WDA2.                           
001400*                                                                         
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLARTC (WDK6) ARTIKELREGISTER CDC          
001700*        PROGRAMMET UPPDATERAR WDK7   ARTIKELREGISTER SDC    LDC          
001800*        PROGRAMMET UPPDATERAR WLINVA (WDH1) INVENTERINGSREGISTER         
001900*        PROGRAMMET UPPDATERAR WLKREE (WDA2) KREDITERINGSREGISTER         
002000*        PROGRAMMET UPPDATERAR WL4121 (WDR4) ÅTERSTARTSREGISTER           
002100*        PROGRAMMET UPPDATERAR WLLOGA (WDL9) SALDOFÖRÄNDRINGAR            
002200*        PROGRAMMET UPPDATERAR WDR5  ATTESTANSVARIGA KREDITNOTOR          
002300*                                    HTYP=4103, SEGMENT=WDGX4103          
002400*                                                                         
002500*                                                                         
002600*    E-TRACKER 1572353 DATUM 20050519                                     
002700*    E-TRACKER 2913019 DATUM 20051215                                     
002800*                                                                         
002900*    ABENDKODER:                                                          
003000*        U0016 -  . . . .                                                 
003100*        U1000 -  . . . .                                                 
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800                                                                          
003900 FILE-CONTROL.                                                            
004000     SKIP2                                                                
004100*          --- INFIL MED UPPDATERINGSPOSTER FRÅN W41830                   
004200     SELECT W41834                     ASSIGN TO W41835D1.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W41834                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY W41834      -L.                                                
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500     SKIP2                                                                
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W4183500'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100*77  IX                          PIC S9(3)   VALUE +0   COMP-3.           
006200 77  IX                          PIC 9(2)    VALUE ZERO.                  
006300 77  IX2                         PIC 9(2)    VALUE ZERO.                  
006400 77  INDX1                       PIC 9(3)    VALUE ZERO.                  
006500 77  MAX-IX                      PIC S9(3)   VALUE +11  COMP-3.           
006600 77  W-KVPOST-IN                 PIC S9(9)   VALUE +0   COMP-3.           
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3)   VALUE +16  COMP-3.           
006800 77  W41834-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W41834                       VALUE 'J'.                   
006901 77  LYNK-NON-API-SW             PIC X       VALUE 'N'.                   
006902     88  LYNK-NON-API                        VALUE 'J'.                   
006910 77  IDTRACK-QTY-SW              PIC X       VALUE 'N'.                   
006920     88  IDTRACK-QTY-DONE                    VALUE 'J'.                   
006930     88  IDTRACK-QTY-NOT-DONE                VALUE 'N'.                   
007000 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
007100     88 EVENT-YES                            VALUE 'LYNK'                 
007200                                                   'POLE'                 
007300                                                   'ECOM'                 
007400                                                   'ACC '                 
007410                                                   'APA '                 
007420                                                   'APB '                 
007430                                                   'APC '                 
007440                                                   'APD '                 
007450                                                   'APE '                 
007460                                                   'APF '                 
007470                                                   'APG '                 
007480                                                   'APH '                 
007490                                                   'API '                 
007491                                                   'APJ '                 
007500                                                   'TAD '.                
007600     88 EVENT-NO                             VALUE '    '.                
007700                                                                          
007800 77  WS-EVENT-KUND               PIC X(1)    VALUE SPACE.                 
007900                                                                          
008000 01  WS-SECTION                  PIC X(32).                               
008100 01  WS-IMS                      PIC X(32).                               
008110 01  WS-KVLEVANM-MXC             PIC S9(7)   VALUE +0 COMP-3.             
008120 01  WS-TEMP-RETMXC              PIC S9(7)   VALUE +0 COMP-3.             
008130 01  WS-TEMP-RETMXCB             PIC S9(7)   VALUE +0 COMP-3.             
008200 01  WS-IDAPIDISCREF.                                                     
008300     03 WS-IDDISTR-EVENT         PIC 9(4).                                
008400     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
008500     03 WS-IDRAPPNR-EVENT        PIC 9(7).                                
008600                                                                          
008700     EJECT                                                                
008800*      --- VALID IDDC CODES                                               
008900*                                                                         
009000*01    -COPY WWDC99                                                       
009100*01    -COPY WWDCKONS                                                     
009200       EJECT                                                              
009300 01  FELTEXT.                                                             
009400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009600                                                                          
009700 01  CHKP-VAR.                                                            
009800    03 CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
009900    03 CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
010000    03 CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
010100    03 CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
010200    03 CHKP-ANT                  PIC S9(3)   VALUE +0.                    
010300    03 CHKP-MAX                  PIC S9(3)   VALUE +100.                  
010400     SKIP2                                                                
010500 01  WS-KLOCKAN                  PIC 9(9)    VALUE ZERO.                  
010600 01  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
010700 01  WS-INV-DAREGDAT-AREA.                                                
010800     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
010900     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
011000       05  WS-INV-NOLL         PIC 9(1).                                  
011100       05  WS-INV-SEKEL        PIC 9(2).                                  
011200       05  WS-INV-AAMMDD       PIC 9(6).                                  
011300                                                                          
011400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011500 01  FILLER REDEFINES DAGENS-DATUM.                                       
011600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011900 01  WS-DATUM.                                                            
012000     03  WS-SEKEL            PIC 9(2).                                    
012100     03  WS-DAT.                                                          
012200      05 WS-AARTAL           PIC 9(2).                                    
012300      05 FILLER              PIC 9(4).                                    
012400 01  WS-DATUM-N  REDEFINES WS-DATUM   PIC 9(8).                           
012500                                                                          
012600 01  WS-TISEGKEYAREA.                                                     
012700     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
012800     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
012900         05  WS-AAR          PIC 9(2).                                    
013000         05  WS-TIAAMMDD     PIC 9(6).                                    
013100         05  WS-LOPNR        PIC 9(1).                                    
013200     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
013300     EJECT                                                                
013400 01  DYNAMISKA-SUBPROGRAM.                                                
013500*                                                                         
013600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014100     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
014200     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
014300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
014400     EJECT                                                                
014500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
014600 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
014700*   -COPY W005WDK7                                                        
014800     EJECT                                                                
014900 01 FILLER                       PIC X(8)    VALUE 'W005WDL7'.            
015000*   -COPY W005WDL7                                                        
015100     EJECT                                                                
015200 01  FILLER                    PIC X(16)  VALUE 'MSG-KOM-WMSGKOM'.        
015300*01  -COPY WMSGKOM  -PRE MSG1-                                            
015400                                                                          
015500 01  FILLER                    PIC X(16)   VALUE 'MSG-IO-AREA'.           
015600*01  -COPY WMSGAREA                                                       
015700                                                                          
015800 01  FILLER                    PIC X(16)   VALUE 'Z430-REQU-AREA'.        
015900*01  -COPY WZ0430I1  -PRE Z430-                                           
016000*    03  -COPY WAPIDISC -RED Z430-REQU-EVENT-DATA -PRE Z430-              
016100                                                                          
016200*    --- PARAMETRAR TILL POSTSUM                                          
016300*                                                                         
016400*01  -COPY W0005   -PRE  POSTSUM-                                         
016500     EJECT                                                                
016600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
016700*01  -COPY WDATAREA                                                       
016800     EJECT                                                                
016900 01  IN-AREA-START               PIC X(24)   VALUE                        
017000                                             'IN-AREA-START'.             
017100     SKIP2                                                                
017200                                                                          
017300*01  AREA -COPY W41834     -PRE IN-                                       
017400*                                                                         
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017700     SKIP3                                                                
017800 01  NYCKLAR-TILL-DLI.                                                    
017900     03  W-IDLEVANM-X.                                                    
018000         05 W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.           
018100         05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.           
018200         05 W-IDRAPPNR           PIC  9(7)   VALUE ZERO.                  
018300                                                                          
018400     03  W-WDA211KY-X.                                                    
018500         05  W-IDARTNR-WDA2      PIC S9(9)   VALUE ZERO COMP-3.           
018600         05  W-IDRADNR-WDA2      PIC S9(5)   VALUE ZERO COMP-3.           
018700                                                                          
018800     03  W-IDARTNR-X.                                                     
018900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019000                                                                          
019100     03  W-KDSEGKEY-X.                                                    
019200         05  W-KDSEGKEY          PIC S9(1)   VALUE ZERO COMP-3.           
019300                                                                          
019400     03  W-IDDC-X.                                                        
019500         05  W-IDDC-WDK7         PIC X(2)    VALUE SPACE.                 
019600                                                                          
019700     03  W-IDHTYP-X.                                                      
019800         05  W-IDHTYP            PIC X(4)    VALUE '4121'.                
019900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
020000                                                                          
020100     03  W-KDSEGKEY-4122-X.                                               
020200         05  W-KDSEGKEY-4122     PIC X(1)    VALUE SPACE.                 
020300     03  W-DAINLEV-X.                                                     
020310         05  W-DAINLEV           PIC 9(16).                               
020400 01  W-WDH111KY-MIN.                                                      
020500     03  IDDC-SEARCH-MIN       PIC X(2).                                  
020600     03  KDINVKAT-SEARCH-MIN   PIC S9(3) VALUE ZERO       COMP-3.         
020700     03  TISEGKEY-SEARCH-MIN   PIC S9(9) VALUE ZERO       COMP-3.         
020800     03  DAREGDAT-SORT-SEARCH-MIN PIC 9(8) VALUE ZERO.                    
020900                                                                          
021000 01  W-WDH111KY-MAX.                                                      
021100     03  IDDC-SEARCH-MAX       PIC X(2).                                  
021200     03  KDINVKAT-SEARCH-MAX   PIC S9(3) VALUE +999       COMP-3.         
021300     03  TISEGKEY-SEARCH-MAX   PIC S9(9) VALUE +999999999 COMP-3.         
021400     03  DAREGDAT-SORT-SEARCH-MAX PIC 9(8) VALUE 99999999.                
021500                                                                          
021600 01  W-WDGXKEY-4103-X.                                                    
021700     03  W-IDHTYP-4103         PIC X(4)    VALUE '4103'.                  
021800     03  W-IDDISTR-4103        PIC S9(5)   VALUE ZERO COMP-3.             
021900     03  W-IDKUNDNR-4103       PIC S9(7)   VALUE ZERO COMP-3.             
022000     03  W-IDRAPPNR-4103       PIC  9(7)   VALUE ZERO.                    
022100     03  FILLER                PIC X(12)   VALUE LOW-VALUE.               
022200                                                                          
022210 01  W-IDGMT-X.                                                           
022220     03  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.                 
022230     03  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.                 
022300     SKIP2                                                                
022400*    --- STATUS-KOD FRÅN IMS                                              
022500 01  STATUS-WS                   PIC XX.                                  
022600     88  SEGMENT-FINNS                       VALUE '  '.                  
022700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
023000     88  IMS-EJ-OK                           VALUE 'XD'.                  
023100     SKIP2                                                                
023200 01  GODK-STATUSKODER.                                                    
023300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023400     SKIP3                                                                
023500 01  SSA1                        PIC X(128).                              
023600 01  SSA2                        PIC X(128).                              
023700     EJECT                                                                
023800*    --- IMS FUNKTIONSKODER                                               
023900*01  -COPY W0003                                                          
024000     EJECT                                                                
024100*    ---  DLI INPUT-OUTPUT AREA                                           
024200                                                                          
024300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024400 01   DLI-IO-AREA-B601.                                                   
024500*     03  -COPY WDB601                                                    
024600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WLKREE01'.            
024700 01  DLI-IO-WLKREE01.                                                     
024800*    03  -COPY WDA201                                                     
024900     EJECT                                                                
025000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WLKREE11'.            
025100 01  DLI-IO-WLKREE11.                                                     
025200*    03  -COPY WDA211                                                     
025300     EJECT                                                                
025400 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WLARTC01'.            
025500 01  DLI-IO-WLARTC01.                                                     
025600*    03  -COPY WDK601                                                     
025700     EJECT                                                                
025800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WLARTC11'.            
025900 01  DLI-IO-WLARTC11.                                                     
026000*    03  -COPY WDK611                                                     
026100     EJECT                                                                
026200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK711'.              
026300 01  DLI-IO-WDK711.                                                       
026400*    03  -COPY WDK711                                                     
026500     EJECT                                                                
026510 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK728'.                      
026520 01  DLI-IO-WDK728.                                                       
026530*    03   -COPY WDK728                                                    
026600 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
026700*01  WLLOGA01  -COPY WDL901                                               
026800*    ---  DLI INPUT-OUTPUT AREA                                           
026900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-2'.        
027000     SKIP3                                                                
027100 01  DLI-IO-AREA-2.                                                       
027200     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
027300     SKIP3                                                                
027400     03  WDGX4122 REDEFINES IO-AREA-2.                                    
027500*        05  -COPY WDGX4122                                               
027600     EJECT                                                                
027700 01  WDH101.                                                              
027800*    03  -COPY WDH101   -PRE INV-                                         
027900 01  WDH111.                                                              
028000*    03  -COPY WDH111                                                     
028100 01  WDH121.                                                              
028200*    03  -COPY WDH121                                                     
028300     EJECT                                                                
028400 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDGX4103'.            
028500 01  DLI-IO-WDGX4103.                                                     
028600*    03  -COPY WDGX4103                                                   
028700     EJECT                                                                
028800 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
028900 01  DLI-IO-OIGA11.                                                       
029000*    03  -COPY WDL711                                                     
029100     EJECT                                                                
029110 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB201'.                      
029120 01  DLI-IO-WDB201.                                                       
029130*    03  -COPY WDB201                                                     
029140     EJECT                                                                
029200 LINKAGE SECTION.                                                         
029300                                                                          
029400*01  -COPY W0009   -PRE MSG-                                              
029500     EJECT                                                                
029600*01  -COPY W0009  -PRE 0693-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE KREE-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE INVA-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE ARTC-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01  -COPY W0008  -PRE WDK7-                                              
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008  -PRE 4121-                                              
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01  -COPY W0008  -PRE WLLOGA-                                            
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700*01  -COPY W0008  -PRE 4103-                                              
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01  -COPY W0008  -PRE OIGA-                                              
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300*01  -COPY W0008  -PRE WDB6-                                              
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032510*01  -COPY W0008  -PRE WDB2-                                              
032520     05  FILLER                  PIC X.                                   
032530     EJECT                                                                
032600 01  WDP8-PCB                    PIC X.                                   
032700     EJECT                                                                
032800 PROCEDURE DIVISION  USING MSG-PCB 0693-PCB KREE-PCB                      
032900     INVA-PCB ARTC-PCB WDK7-PCB 4121-PCB WLLOGA-PCB 4103-PCB              
033000     OIGA-PCB WDB6-PCB WDB2-PCB WDP8-PCB.                                 
033100     ENTRY 'DLITCBL' USING MSG-PCB 0693-PCB KREE-PCB                      
033200     INVA-PCB ARTC-PCB WDK7-PCB 4121-PCB WLLOGA-PCB 4103-PCB              
033300     OIGA-PCB WDB6-PCB WDB2-PCB WDP8-PCB.                                 
033400                                                                          
033500     SKIP2                                                                
033600     PERFORM A-INIT                                                       
033700                                                                          
033800     PERFORM IMS-LAS-ATERSTART                                            
033900     IF 4122-KVPOST > +0                                                  
034000       PERFORM S11-LAS-FRAM-TILL-CHKPOINT                                 
034100     ELSE                                                                 
034200       PERFORM S01-LAES-W41834                                            
034300     END-IF                                                               
034400                                                                          
034500     PERFORM UNTIL END-OF-W41834                                          
034600       IF CHKP-ANT > CHKP-MAX                                             
034700         PERFORM X-TAG-CHECKPOINT                                         
034800       END-IF                                                             
034900                                                                          
035000       EVALUATE IN-IDPTYP                                                 
035100          WHEN '001'                                                      
035200             PERFORM B-TILLAEGGSFAKTURA                                   
035300          WHEN '003'                                                      
035400             PERFORM D-RETILL-UPD-KREE                                    
035500          WHEN '004'                                                      
035600             PERFORM E-KNOTA-UPD-KREE                                     
035700          WHEN '005'                                                      
035800             PERFORM F-KNOTA-UPD-KVLS                                     
035900          WHEN '006'                                                      
036000             PERFORM E-KNOTA-UPD-KREE                                     
036100          WHEN '007'                                                      
036200             PERFORM G-INVENTERING-UPD-ROT                                
036300          WHEN '008'                                                      
036400             PERFORM H-INVENTERING-UPD-RAD                                
036500          WHEN '009'                                                      
036600             PERFORM J-UPD-IDDC-RET-WDA201                                
036700          WHEN '010'                                                      
036800             PERFORM I-SAETT-STATUS                                       
036900          WHEN '011'                                                      
037000             PERFORM K-UPD-IDDC-RET-WDA211                                
037100       END-EVALUATE                                                       
037200                                                                          
037300       PERFORM S01-LAES-W41834                                            
037400     END-PERFORM                                                          
037500                                                                          
037600                                                                          
037700     PERFORM Z-FINIT                                                      
037800                                                                          
037900     MOVE ZERO TO RETURN-CODE                                             
038000     GOBACK                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 A-INIT SECTION.                                                          
038400     SKIP2                                                                
038500                                                                          
038600     PERFORM IMS-RESTART                                                  
038700                                                                          
038800     OPEN INPUT W41834                                                    
038900                                                                          
038910     MOVE NEJ                          TO LYNK-NON-API-SW                 
039000                                                                          
039100     MOVE +0                           TO CHKP-ANT                        
039200                                          W-KVPOST-IN                     
039300     MOVE IDPGM                        TO POSTSUM-PROGNAMN                
039400     ACCEPT DAGENS-DATUM FROM DATE                                        
039500     ACCEPT WS-KLOCKAN   FROM TIME                                        
039600                                                                          
039700     MOVE DAGENS-DATUM   TO WS-DAT                                        
039800     IF WS-AARTAL < 50                                                    
039900       MOVE 20           TO WS-SEKEL                                      
040000     ELSE                                                                 
040100       MOVE 19           TO WS-SEKEL                                      
040200     END-IF                                                               
040300     MOVE DAGENS-DATUM   TO WS-INV-AAMMDD                                 
040400     MOVE WS-SEKEL       TO WS-INV-SEKEL                                  
040500                                                                          
040600*    -- INITIALIZE W006KOM                                                
040700     MOVE SPACE                      TO MSG1-MSG-KOM-WMSGKOM              
040800     MOVE SPACE                      TO MSG1-MSG-KOM-KDTRANS              
040900     MOVE SPACE                      TO MSG1-MSG-KOM-IDMFSMED             
041000     MOVE LENGTH OF MSG1-MSG-KOM-WMSGKOM TO MSG1-MSG-KOM-KVLL             
041100     MOVE LOW-VALUE                  TO MSG1-MSG-KOM-KDZ1                 
041200     MOVE LOW-VALUE                  TO MSG1-MSG-KOM-KDZ2                 
041300     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG1-MSG-KOM-TIREGDAT             
041400     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG1-MSG-KOM-TIKLOCK              
041500     MOVE LOW-VALUE                  TO MSG-KDZ1                          
041600     MOVE LOW-VALUE                  TO MSG-KDZ2                          
041700*                                                                         
041800     .                                                                    
041900     EJECT                                                                
042000 B-TILLAEGGSFAKTURA SECTION.                                              
042100                                                                          
042200     MOVE IN-IDLEVANM                  TO W-IDLEVANM-X                    
042300     MOVE IN-IDARTNR                   TO W-IDARTNR-WDA2                  
042400     MOVE IN-IDRADNR                   TO W-IDRADNR-WDA2                  
042500                                                                          
042600     PERFORM IMS-GHU-KREE-LEV                                             
042700                                                                          
042800     MOVE IN-IDKNOTNR                  TO LEV-IDKNOTNR                    
042900     MOVE IN-KDFAKTYP-KNOT             TO LEV-KDFAKTYP-KNOT               
043000     MOVE IN-TIKNOTA                   TO LEV-TIKNOTA                     
043100                                                                          
043200     PERFORM IMS-REPL-KREE-LEV                                            
043300     .                                                                    
043400     EJECT                                                                
043500 D-RETILL-UPD-KREE SECTION.                                               
043600                                                                          
043700     MOVE IN-IDLEVANM                  TO W-IDLEVANM-X                    
043800                                                                          
043900     PERFORM IMS-GHU-KREE-ANM                                             
044000                                                                          
044100     MOVE IN-TIRETILL                  TO ANM-DARETILL                    
044200     IF IN-TIRETILL NOT = ZERO                                            
044300       IF IN-TIRETILL < 500000                                            
044400         MOVE 20                       TO ANM-DARETILL (1:2)              
044500       ELSE                                                               
044600         IF IN-TIRETILL < 999999                                          
044700           MOVE 19                     TO ANM-DARETILL (1:2)              
044800         ELSE                                                             
044900           MOVE 99999999               TO ANM-DARETILL                    
045000         END-IF                                                           
045100       END-IF                                                             
045200     END-IF                                                               
045300                                                                          
045400     PERFORM IMS-REPL-KREE-ANM                                            
045500     .                                                                    
045600     EJECT                                                                
045700 E-KNOTA-UPD-KREE SECTION.                                                
045800                                                                          
045900     MOVE IN-IDLEVANM                  TO W-IDLEVANM-X                    
046000     MOVE IN-IDARTNR                   TO W-IDARTNR-WDA2                  
046100     MOVE IN-IDRADNR                   TO W-IDRADNR-WDA2                  
046200                                                                          
046300     PERFORM IMS-GHU-KREE-LEV                                             
046400                                                                          
046500     IF IN-IDPTYP = '004'                                                 
046600        MOVE IN-IDKNOTNR               TO LEV-IDKNOTNR                    
046700        MOVE IN-KDFAKTYP-KNOT          TO LEV-KDFAKTYP-KNOT               
046800        MOVE IN-TIKNOTA                TO LEV-TIKNOTA                     
046900        MOVE IN-PRARTBTO               TO LEV-PRARTBTO                    
047000     ELSE                                                                 
047100        MOVE IN-IDKNOTNR               TO LEV-IDKNOTNR                    
047200        MOVE IN-KDFAKTYP-KNOT          TO LEV-KDFAKTYP-KNOT               
047300        MOVE IN-TIKNOTA                TO LEV-TIKNOTA                     
047400     END-IF                                                               
047500                                                                          
047600     PERFORM IMS-REPL-KREE-LEV                                            
047700     .                                                                    
047800     EJECT                                                                
047900 F-KNOTA-UPD-KVLS SECTION.                                                
048000                                                                          
048100     MOVE IN-IDARTNR                   TO W-IDARTNR                       
048200     MOVE IN-IDDC                      TO W-IDDC-WDK7                     
048300                                          WS-IDDC                         
048400     IF CDC-SE                                                            
048500        PERFORM IMS-GU-ARTC-ART                                           
048600        PERFORM IMS-GHNP-ARTC-CLAG                                        
048700        IF IN-KDAVVTYP = +1                                               
048800           COMPUTE CLAG-KVLS = CLAG-KVLS + IN-KVLEVANM                    
048900           PERFORM IMS-REPL-ARTC                                          
049000        ELSE                                                              
049100           COMPUTE CLAG-KVLS = CLAG-KVLS - IN-KVLEVANM                    
049200           PERFORM IMS-REPL-ARTC                                          
049300        END-IF                                                            
049400        PERFORM FA-BERAKNA-SALDOLOGG-DATA                                 
049500     ELSE                                                                 
049600        PERFORM IMS-GET-HOLD-SALDON-WDK711                                
049700        IF IN-KDAVVTYP = +1                                               
049800           IF SEGMENT-SAKNAS                                              
049900              PERFORM S20-LAEGG-UPP-NY-WDK7                               
050000              COMPUTE SLAG-KVLS = SLAG-KVLS + IN-KVLEVANM                 
050100           ELSE                                                           
050200              COMPUTE SLAG-KVLS = SLAG-KVLS + IN-KVLEVANM                 
050300              PERFORM IMS-REPL-SALDON-WDK711                              
050310              IF NDC-MX                                                   
050320               MOVE 9999999999999999    TO W-DAINLEV                      
050330               MOVE IN-KVLEVANM         TO WS-KVLEVANM-MXC                
050340               MOVE ZERO                TO WS-TEMP-RETMXC                 
050350                                           WS-TEMP-RETMXCB                
050360               PERFORM IMS-GU-WDK711                                      
050370               PERFORM IMS-GHNP-WDK728-LAST                               
050380               PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE           
050390                IF TRCK-KVTRACK-KVAR < TRCK-KVANTMOT                      
050391                 COMPUTE WS-TEMP-RETMXCB = WS-TEMP-RETMXCB +              
050392                                           WS-TEMP-RETMXC                 
050393                 COMPUTE WS-TEMP-RETMXC = WS-TEMP-RETMXC +                
050394                         (TRCK-KVANTMOT - TRCK-KVTRACK-KVAR)              
050395                 IF WS-TEMP-RETMXC <= WS-KVLEVANM-MXC                     
050396                  MOVE TRCK-KVANTMOT TO TRCK-KVTRACK-KVAR                 
050397                  PERFORM IMS-REPL-WDK728                                 
050398                 ELSE                                                     
050399                  COMPUTE WS-TEMP-RETMXCB = WS-KVLEVANM-MXC -             
050400                                            WS-TEMP-RETMXCB               
050401                  ADD WS-TEMP-RETMXCB TO TRCK-KVTRACK-KVAR                
050402                  PERFORM IMS-REPL-WDK728                                 
050403                  MOVE 'J' TO IDTRACK-QTY-SW                              
050404                 END-IF                                                   
050405                END-IF                                                    
050406                IF WS-TEMP-RETMXC = WS-KVLEVANM-MXC                       
050407                 MOVE 'J' TO IDTRACK-QTY-SW                               
050408                END-IF                                                    
050409                IF IDTRACK-QTY-NOT-DONE                                   
050410                 PERFORM IMS-GU-WDK711                                    
050411                 MOVE  TRCK-DAINLEV TO W-DAINLEV                          
050412                 PERFORM IMS-GHNP-WDK728-LAST                             
050413                END-IF                                                    
050414               END-PERFORM                                                
050415              END-IF                                                      
050420           END-IF                                                         
050500        ELSE                                                              
050600           IF SEGMENT-SAKNAS                                              
050700              PERFORM S20-LAEGG-UPP-NY-WDK7                               
050800              COMPUTE SLAG-KVLS = SLAG-KVLS - IN-KVLEVANM                 
050900           ELSE                                                           
051000              COMPUTE SLAG-KVLS = SLAG-KVLS - IN-KVLEVANM                 
051100              PERFORM IMS-REPL-SALDON-WDK711                              
051110              IF NDC-MX                                                   
051120               PERFORM IMS-GU-WDK711                                      
051130               MOVE 9999999999999999    TO W-DAINLEV                      
051140               MOVE IN-KVLEVANM         TO WS-KVLEVANM-MXC                
051150               PERFORM IMS-GHNP-WDK728-LAST                               
051160               PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE           
051170                IF TRCK-KVTRACK-KVAR <= TRCK-KVANTMOT AND                 
051180                   TRCK-KVTRACK-KVAR NOT = ZERO                           
051190                   COMPUTE WS-KVLEVANM-MXC = WS-KVLEVANM-MXC -            
051191                                            TRCK-KVTRACK-KVAR             
051192                   IF WS-KVLEVANM-MXC >= ZERO                             
051193                    MOVE ZERO TO TRCK-KVTRACK-KVAR                        
051194                    PERFORM IMS-REPL-WDK728                               
051195                   ELSE                                                   
051196                    COMPUTE TRCK-KVTRACK-KVAR =                           
051197                            TRCK-KVTRACK-KVAR - WS-KVLEVANM-MXC           
051198                    PERFORM IMS-REPL-WDK728                               
051199                    MOVE 'J' TO IDTRACK-QTY-SW                            
051200                   END-IF                                                 
051201                END-IF                                                    
051202                IF WS-KVLEVANM-MXC = ZERO                                 
051203                 MOVE 'J' TO IDTRACK-QTY-SW                               
051204                END-IF                                                    
051205                IF IDTRACK-QTY-NOT-DONE                                   
051206                 PERFORM IMS-GU-WDK711                                    
051207                 MOVE  TRCK-DAINLEV TO W-DAINLEV                          
051208                 PERFORM IMS-GHNP-WDK728-LAST                             
051209                END-IF                                                    
051210               END-PERFORM                                                
051211             END-IF                                                       
051220           END-IF                                                         
051300        END-IF                                                            
051420        PERFORM S13-BERAKNA-SALDOLOGG-DATA                                
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800 FA-BERAKNA-SALDOLOGG-DATA SECTION.                                       
051900     PERFORM S14-FLYTTA-SALDOLOGG-DATA                                    
052000     MOVE IN-IDARTNR     TO LOGG-IDARTNR                                  
052100     MOVE WC-CDC-SE      TO LOGG-IDDC                                     
052200     MOVE CLAG-KVLS      TO LOGG-KVLS                                     
052300     MOVE CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                                
052400     MOVE CLAG-KVEFRS    TO LOGG-KVEFRS                                   
052500     COMPUTE LOGG-KVAKS  =  CLAG-KVAKS-CDC                                
052600                         +  CLAG-KVAKS-T                                  
052700*  ---KOLLAR FÖRÄNDRINGAR PÅ WDK611 OCH                                   
052800*  ---LOGGAR DESSA PÅ WDL9                                                
052900     IF IN-KDAVVTYP = +1                                                  
053000        MOVE '+'        TO LOGG-IDTECKEN-KVLS                             
053100     ELSE                                                                 
053200        MOVE '-'        TO LOGG-IDTECKEN-KVLS                             
053300     END-IF                                                               
053400     PERFORM S15-ISRT-SALDOLOGG                                           
053500     .                                                                    
053600     EJECT                                                                
053700 G-INVENTERING-UPD-ROT SECTION.                                           
053800     MOVE 'G-INV-UPP-ROT' TO WS-SECTION                                   
053900     MOVE IN-IDARTNR                   TO INV-ART-IDARTNR                 
054000                                                                          
054100     PERFORM IMS-ISRT-WDH101                                              
054200     .                                                                    
054300     EJECT                                                                
054400 H-INVENTERING-UPD-RAD SECTION.                                           
054500     MOVE 'H-INV-UPP-RAD' TO WS-SECTION                                   
054600     MOVE IN-IDARTNR                   TO W-IDARTNR                       
054700     MOVE IN-IDDC                      TO IDDC-SEARCH-MAX                 
054800                                          IDDC-SEARCH-MIN                 
054900     PERFORM IMS-GNP-INVA-INVA11                                          
055000     IF SEGMENT-SAKNAS                                                    
055100                                                                          
055200       PERFORM IMS-GU-ARTC-ART                                            
055300       MOVE ART-IDFKNGRP                 TO INV-IDFKNGRP                  
055400       MOVE ART-KDPRODSL                 TO INV-KDPRODSL                  
055500                                                                          
055600       PERFORM IMS-GHNP-ARTC-CLAG                                         
055700       MOVE CLAG-KDPSLLOC                TO INV-KDPSLLOC                  
055800       MOVE CLAG-KDVVKL                  TO INV-KDVVKL                    
055900                                                                          
056000       MOVE IN-IDDC                      TO WS-IDDC                       
056100       IF CDC-SE                                                          
056200         MOVE CLAG-ADLAGOMR              TO INV-ADLAGOMR                  
056300         MOVE CLAG-ADGANG                TO INV-ADGANG                    
056400         MOVE CLAG-ADPLATS               TO INV-ADPLATS                   
056500       ELSE                                                               
056600         MOVE IN-IDDC                    TO W-IDDC-WDK7                   
056700         PERFORM IMS-GET-HOLD-SALDON-WDK711                               
056800         IF SEGMENT-SAKNAS                                                
056900           MOVE ZERO                     TO INV-ADLAGOMR                  
057000                                            INV-ADGANG                    
057100                                            INV-ADPLATS                   
057200         ELSE                                                             
057300           MOVE SLAG-ADLAGOMR            TO INV-ADLAGOMR                  
057400           MOVE SLAG-ADGANG              TO INV-ADGANG                    
057500           MOVE SLAG-ADPLATS             TO INV-ADPLATS                   
057600         END-IF                                                           
057700       END-IF                                                             
057800       MOVE NEJ                          TO INV-FLINVBEH                  
057900       MOVE NEJ                          TO INV-FLINVSKR                  
058000       MOVE NEJ                          TO INV-FLINV2B                   
058100       MOVE NEJ                          TO INV-FLINV2C                   
058200       MOVE NEJ                          TO INV-FLINV2D                   
058300       MOVE NEJ                          TO INV-FLINV3E                   
058400       MOVE NEJ                          TO INV-FLINV4N                   
058500       MOVE NEJ                          TO INV-FLINV4P                   
058600       MOVE NEJ                          TO INV-FLINV4R                   
058700       MOVE 0                            TO INV-KDINVKAT-OLD              
058800       MOVE NEJ                          TO INV-FLINV85                   
058900       MOVE SPACE                        TO INV-FILLER1                   
059000                                            INV-FILLER2                   
059100       MOVE IN-IDDC                      TO INV-IDDC                      
059200       MOVE +2                           TO INV-KDINVPRIO                 
059300       MOVE IN-KDINVKAT                  TO INV-KDINVKAT                  
059400       MOVE IN-KVJUSTKV                  TO INV-KVJUSTKV                  
059500       MOVE IN-TEINVANM                  TO INV-TEINVANM                  
059600       MOVE ZERO                         TO INV-IDPRTOMG                  
059700                                            INV-IDLOPNR                   
059800                                            INV-KVAKS-OLD                 
059900                                            INV-KVEFRS-OLD                
060000                                            INV-KVLS-OLD                  
060100                                            INV-DAREGDAT-PR1              
060200                                            INV-DAREGDAT-PR2              
060300                                            INV-DAREGDAT-PR3              
060400                                                                          
060500       MOVE WS-SEKEL                     TO WS-AAR                        
060600       MOVE DAGENS-DATUM                 TO WS-TIAAMMDD                   
060700       MOVE 0                            TO WS-LOPNR                      
060800       MOVE WS-TIAAAAMMDDL               TO INV-TISEGKEY                  
060900       MOVE WS-INV-DAREGDAT              TO INV-DAREGDAT-CRE              
061000       MOVE WS-INV-DAREGDAT              TO INV-DAREGDAT                  
061100**     COMPUTE INV-DAREGDAT-SORT =                                        
061200**      99999999 - WS-INV-DAREGDAT                                        
061300       MOVE 99999999             TO INV-DAREGDAT-SORT                     
061400       PERFORM IMS-ISRT-WDH111                                            
061500                                                                          
061600       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
061700         IF SEGMENT-FINNS-REDAN                                           
061800           ADD 1 TO INV-TISEGKEY                                          
061900           PERFORM IMS-ISRT-WDH111                                        
062000         END-IF                                                           
062100       END-PERFORM                                                        
062200                                                                          
062300*** INSERT PÅ WDH121 SEGMENTET ***                                        
062400       MOVE 'W4183500'         TO INVL-IDUSER                             
062500       MOVE '0'                TO INVL-KDSEGKEY                           
062600       PERFORM IMS-ISRT-WDH121                                            
062700       MOVE SPACE              TO INVL-IDUSER                             
062800       MOVE '1'                TO INVL-KDSEGKEY                           
062900       PERFORM IMS-ISRT-WDH121                                            
063000       MOVE SPACE              TO INVL-IDUSER                             
063100       MOVE '2'                TO INVL-KDSEGKEY                           
063200       PERFORM IMS-ISRT-WDH121                                            
063300       MOVE SPACE              TO INVL-IDUSER                             
063400       MOVE '3'                TO INVL-KDSEGKEY                           
063500       PERFORM IMS-ISRT-WDH121                                            
063600**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 I-SAETT-STATUS SECTION.                                                  
064100                                                                          
064200     MOVE IN-IDLEVANM                  TO W-IDLEVANM-X                    
064300     MOVE IN-IDDISTR                   TO W-IDDISTR-4103                  
064400     MOVE IN-IDKUNDNR                  TO W-IDKUNDNR-4103                 
064500     MOVE IN-IDRAPPNR                  TO W-IDRAPPNR-4103                 
064600                                                                          
064700     PERFORM IMS-GHU-KREE-ANM                                             
064800                                                                          
064900     IF IN-KDLEVANM = '4'                                                 
065000        MOVE IN-IDPERSON               TO ANM-IDPERSON                    
065100        MOVE IN-KDARBTYP               TO ANM-KDARBTYP                    
065200        MOVE IN-KVRADER-RT             TO ANM-KVRADER-RT                  
065300                                          ANM-KVRADER-OBEH                
065400     END-IF                                                               
065500                                                                          
065600     MOVE IN-KDLEVANM                  TO ANM-KDLEVANM                    
065700     MOVE IN-FLFARLIG                  TO ANM-FLFARLIG                    
065800     MOVE ANM-IDSYSTEM                 TO EVENT-SW                        
065900                                          WS-EVENT-KUND                   
066000                                          Z430-REQU-IDEVENTREC            
066100                                                                          
066200     PERFORM IMS-REPL-KREE-ANM                                            
066300                                                                          
066400     IF IN-KDLEVANM = '8'                                                 
066500       MOVE IN-IDDISTR         TO WS-IDDISTR-EVENT                        
066600       MOVE IN-IDKUNDNR        TO WS-IDKUNDNR-EVENT                       
066700       MOVE IN-IDRAPPNR        TO WS-IDRAPPNR-EVENT                       
066800                                                                          
066810       PERFORM S02-CHK-LYN-NONAPI                                         
066900        IF EVENT-YES OR LYNK-NON-API                                      
066901                                                                          
066910           IF LYNK-NON-API                                                
066920              MOVE 'LYNK'          TO Z430-REQU-IDEVENTREC                
066930           ELSE                                                           
066940              MOVE ANM-IDSYSTEM    TO Z430-REQU-IDEVENTREC                
066950           END-IF                                                         
067000                                                                          
067100           IF (ANM-IDSYSTEM = 'LYNK') OR LYNK-NON-API                     
067200             MOVE 'L'              TO WS-EVENT-KUND                       
067300           END-IF                                                         
067400           IF ANM-IDSYSTEM = 'POLE'                                       
067500             MOVE 'P'              TO WS-EVENT-KUND                       
067600           END-IF                                                         
067700           IF ANM-IDSYSTEM = 'ECOM'                                       
067800             MOVE 'E'              TO WS-EVENT-KUND                       
067900           END-IF                                                         
068000           IF ANM-IDSYSTEM = 'TAD '                                       
068100             MOVE 'T'              TO WS-EVENT-KUND                       
068200           END-IF                                                         
068300           IF ANM-IDSYSTEM = 'ACC '                                       
068310             MOVE 'A'              TO WS-EVENT-KUND                       
068320           END-IF                                                         
068350           IF ANM-IDSYSTEM = 'APA '                                       
068360             MOVE 'K'              TO WS-EVENT-KUND                       
068361           END-IF                                                         
068370           IF ANM-IDSYSTEM = 'APB '                                       
068380             MOVE 'B'              TO WS-EVENT-KUND                       
068381           END-IF                                                         
068390           IF ANM-IDSYSTEM = 'APC '                                       
068391             MOVE 'C'              TO WS-EVENT-KUND                       
068392           END-IF                                                         
068393           IF ANM-IDSYSTEM = 'APD '                                       
068394             MOVE 'D'              TO WS-EVENT-KUND                       
068395           END-IF                                                         
068396           IF ANM-IDSYSTEM = 'APE '                                       
068397             MOVE 'M'              TO WS-EVENT-KUND                       
068398           END-IF                                                         
068399           IF ANM-IDSYSTEM = 'APF '                                       
068400             MOVE 'F'              TO WS-EVENT-KUND                       
068401           END-IF                                                         
068402           IF ANM-IDSYSTEM = 'APG '                                       
068403             MOVE 'G'              TO WS-EVENT-KUND                       
068404           END-IF                                                         
068405           IF ANM-IDSYSTEM = 'APH '                                       
068406             MOVE 'H'              TO WS-EVENT-KUND                       
068407           END-IF                                                         
068408           IF ANM-IDSYSTEM = 'API '                                       
068409             MOVE 'I'              TO WS-EVENT-KUND                       
068410           END-IF                                                         
068411           IF ANM-IDSYSTEM = 'APJ '                                       
068412             MOVE 'J'              TO WS-EVENT-KUND                       
068413           END-IF                                                         
068420           PERFORM L-CREATE-EVENT                                         
068500        END-IF                                                            
068600                                                                          
068700       PERFORM IMS-GHU-WDGX4103                                           
068800       IF SEGMENT-FINNS                                                   
068900         PERFORM IMS-DLET-WDGX4103                                        
069000       END-IF                                                             
069100     END-IF                                                               
069200     .                                                                    
069300     EJECT                                                                
069400 J-UPD-IDDC-RET-WDA201    SECTION.                                        
069500                                                                          
069600     MOVE IN-IDLEVANM                  TO W-IDLEVANM-X                    
069700                                                                          
069800     PERFORM IMS-GHU-KREE-ANM                                             
069900                                                                          
070000     MOVE IN-IDDC-RET                  TO ANM-IDDC-RET                    
070100     MOVE IN-IXDCCLEAR                 TO ANM-IXDCCLEAR                   
070200                                                                          
070300     PERFORM IMS-REPL-KREE-ANM                                            
070400     .                                                                    
070500     EJECT                                                                
070600 K-UPD-IDDC-RET-WDA211    SECTION.                                        
070700                                                                          
070800     MOVE IN-IDLEVANM                  TO W-IDLEVANM-X                    
070900     MOVE IN-IDARTNR                   TO W-IDARTNR-WDA2                  
071000     MOVE IN-IDRADNR                   TO W-IDRADNR-WDA2                  
071100                                                                          
071200     PERFORM IMS-GHU-KREE-LEV                                             
071300                                                                          
071400     MOVE IN-IDDC-RET                  TO LEV-IDDC-RET                    
071500                                                                          
071600     PERFORM IMS-REPL-KREE-LEV                                            
071700     .                                                                    
071800     EJECT                                                                
071900 L-CREATE-EVENT SECTION.                                                  
072000                                                                          
072100     MOVE '001'                      TO Z430-REQU-IDMSGVER                
072200     MOVE 'Discrepancy'              TO Z430-REQU-IDEVENT                 
072300     MOVE 'UPDATE'                   TO Z430-REQU-IDEVENTTYP              
072400     MOVE FUNCTION CURRENT-DATE      TO Z430-REQU-TIMESTAMP               
072500     MOVE 'WAPIDISC'                 TO Z430-REQU-IDCPYTXT                
072600     MOVE WS-IDAPIDISCREF            TO Z430-IDAPIDISCREF                 
072700     MOVE '159'                      TO Z430-IDMSG                        
072800     MOVE 'Return goods is Binned'   TO Z430-TEMFSINF                     
072900                                                                          
073000     MOVE 'WZ0430X '                 TO MSG-KDTRANS-1                     
073100     MOVE 'Z430'                     TO MSG-IDTRANS-1                     
073200     MOVE '1'                        TO MSG-KDMFSFOR-1                    
073300     MOVE 'W4183500'                 TO MSG1-MSG-KOM-IDSNDJOB             
073400     MOVE 'WZ0430I1'                 TO MSG1-MSG-KOM-IDCPYTXT             
073500                                                                          
073600*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
073700*    -- IDSNDNOD REFER AS EVE-XXXX (XXX -CUSTOMER DETAILS)                
073800*    -- IDCPYTXT REFER AS RETURN COPYBOOK                                 
073900**   MOVE 'WAPIDISC'          TO MSG1-MSG-KOM-IDCPYTXT                    
074000     STRING 'EVE' WS-EVENT-KUND WS-IDDISTR-EVENT                          
074100          DELIMITED BY SIZE INTO MSG1-MSG-KOM-IDSNDNOD                    
074200                                                                          
074300     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
074400     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
074500                                                                          
074600     CALL W006KOM USING MSG-PCB                                           
074700                        0693-PCB                                          
074800                        WDP8-PCB                                          
074900                        MSG1-MSG-KOM-WMSGKOM                              
075000                        MSG-IO-AREA                                       
075100     IF MSG1-MSG-KOM-IDMFSMED NOT = SPACE                                 
075200        MOVE                                                              
075300        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
075400                                     TO FELTEXT                           
075500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 Z-FINIT SECTION.                                                         
076000                                                                          
076100                                                                          
076200     CLOSE W41834                                                         
076300                                                                          
076400     PERFORM S12-NOLLA-ATERSTART                                          
076500                                                                          
076600     MOVE 'S'                          TO POSTSUM-OPKOD                   
076700     CALL POSTSUM USING POSTSUM-PARM                                      
076800     .                                                                    
076900     EJECT                                                                
077000 S01-LAES-W41834  SECTION.                                                
077100     SKIP2                                                                
077200     READ W41834 INTO IN-AREA                                             
077300     AT END                                                               
077400        SET END-OF-W41834              TO TRUE                            
077500                                                                          
077600     NOT AT END                                                           
077700        MOVE 'W41835'                  TO POSTSUM-FDNAMN                  
077800        MOVE 'W41835D1'                TO POSTSUM-DDNAMN2                 
077900        MOVE IN-IDPTYP                 TO POSTSUM-TRANSTYP                
078000        CALL POSTSUM USING POSTSUM-PARM                                   
078100                                                                          
078200        ADD 1                          TO W-KVPOST-IN                     
078300                                          CHKP-ANT                        
078400     END-READ                                                             
078500     .                                                                    
078600     EJECT                                                                
078700 S02-CHK-LYN-NONAPI SECTION.                                              
078800                                                                          
078801     MOVE IN-IDDISTR                   TO W-IDDISTR-WDB2                  
078802     MOVE IN-IDKUNDNR                  TO W-IDKUNDNR-WDB2                 
078803     MOVE NEJ                          TO LYNK-NON-API-SW                 
078804                                                                          
078805     PERFORM IMS-GU-WDB201                                                
078806                                                                          
078807     IF SEGMENT-FINNS                                                     
078808       IF GMT-KDKUNDKAT = 03                                              
078809          MOVE JA                      TO LYNK-NON-API-SW                 
078810       END-IF                                                             
078811     END-IF                                                               
078812     .                                                                    
078820     EJECT                                                                
078830 S11-LAS-FRAM-TILL-CHKPOINT SECTION.                                      
078840                                                                          
078900     PERFORM UNTIL END-OF-W41834 OR                                       
079000                    W-KVPOST-IN = 4122-KVPOST                             
079100        PERFORM S01-LAES-W41834                                           
079200     END-PERFORM                                                          
079300                                                                          
079400     IF END-OF-W41834                                                     
079500        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
079600                                       TO FELTEXT                         
079700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
079800     END-IF                                                               
079900     .                                                                    
080000     EJECT                                                                
080100 S12-NOLLA-ATERSTART SECTION.                                             
080200                                                                          
080300     PERFORM IMS-LAS-ATERSTART                                            
080400                                                                          
080500     MOVE +0                           TO 4122-KVPOST                     
080600     MOVE DAGENS-DATUM                 TO 4122-TIUPPDAT                   
080700     ACCEPT 4122-TIUPPTID FROM TIME                                       
080800     MOVE +0                           TO 4122-IDDISTR                    
080900     MOVE +0                           TO 4122-IDKUNDNR                   
081000     MOVE ZERO                         TO 4122-IDRAPPNR                   
081100                                                                          
081200     PERFORM IMS-REPL-ATERSTART                                           
081300                                                                          
081400     .                                                                    
081500     EJECT                                                                
081600 S13-BERAKNA-SALDOLOGG-DATA SECTION.                                      
081700     PERFORM S14-FLYTTA-SALDOLOGG-DATA                                    
081800*    ---KOLLAR SALDOFÖRÄNDRINGAR PÅ WDK711 OCH                            
081900*    ---LOGGAR DESSA PÅ WDL9                                              
082000     MOVE IN-IDARTNR          TO LOGG-IDARTNR                             
082100     MOVE IN-IDDC             TO LOGG-IDDC                                
082200     MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
082300     MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                              
082400     MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                               
082500     MOVE SLAG-KVLS           TO LOGG-KVLS                                
082600     IF IN-KDAVVTYP = +1                                                  
082700        MOVE '+'              TO LOGG-IDTECKEN-KVLS                       
082800     ELSE                                                                 
082900        MOVE '-'              TO LOGG-IDTECKEN-KVLS                       
083000     END-IF                                                               
083100     PERFORM S15-ISRT-SALDOLOGG                                           
083200     .                                                                    
083300     EJECT                                                                
083400 S14-FLYTTA-SALDOLOGG-DATA SECTION.                                       
083500     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
083600     ACCEPT WS-KLOCKAN               FROM TIME                            
083700     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
083800     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-KLOCKAN                
083900     MOVE 9                   TO LOGG-IDSEKVNR                            
084000     MOVE 'DISC'              TO LOGG-IDHUVTYP                            
084100     MOVE 'DIS'               TO LOGG-IDSUBTYP                            
084200     MOVE 'W4183500'          TO LOGG-IDPGM                               
084300     MOVE SPACE               TO LOGG-IDTRANS                             
084400     MOVE 'W4183500'          TO LOGG-IDUSER                              
084500     MOVE SPACE               TO LOGG-REF                                 
084600     MOVE IN-IDDISTR          TO LOGG-IDDISTR                             
084700     MOVE IN-IDKUNDNR         TO LOGG-IDKUNDNR                            
084800     MOVE IN-IDRAPPNR         TO LOGG-IDRAPPNR                            
084900     MOVE IN-KVLEVANM         TO LOGG-KVART-SALDO                         
085000     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
085100     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
085200     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
085300     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
085400     .                                                                    
085500     EJECT                                                                
085600 S15-ISRT-SALDOLOGG SECTION.                                              
085700     PERFORM IMS-ISRT-WDL901                                              
085800     IF SEGMENT-FINNS-REDAN                                               
085900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
086000         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
086100         PERFORM IMS-ISRT-WDL901                                          
086200       END-PERFORM                                                        
086300     END-IF                                                               
086400     .                                                                    
086500     EJECT                                                                
086600 S20-LAEGG-UPP-NY-WDK7 SECTION.                                           
086700                                                                          
086800     MOVE ALL '+'      TO WDK7-W005WDK7                                   
086900     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
087000     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
087100     MOVE IN-IDDC      TO WDK7-IDDC-KFB                                   
087200                          WDK7-IDDC                                       
087300                                                                          
087400     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB                  
087500                                       WDK7-PCB                           
087600     MOVE WDK7-WDK711  TO SLAG-WDK711                                     
087700     .                                                                    
087800     EJECT                                                                
087900                                                                          
088000 X-TAG-CHECKPOINT   SECTION.                                              
088100                                                                          
088200     PERFORM IMS-LAS-ATERSTART                                            
088300                                                                          
088400     MOVE W-KVPOST-IN                  TO 4122-KVPOST                     
088500     MOVE DAGENS-DATUM                 TO 4122-TIUPPDAT                   
088600     ACCEPT 4122-TIUPPTID    FROM TIME                                    
088700     MOVE IN-IDDISTR                   TO 4122-IDDISTR                    
088800     MOVE IN-IDKUNDNR                  TO 4122-IDKUNDNR                   
088900     MOVE IN-IDRAPPNR                  TO 4122-IDRAPPNR                   
089000                                                                          
089100     PERFORM IMS-REPL-ATERSTART                                           
089200     PERFORM IMS-CHECKPOINT                                               
089300     MOVE ZERO                         TO CHKP-ANT                        
089400     .                                                                    
089500     EJECT                                                                
089600* --- IMS SEKTIONER ---                                                   
089700     SKIP3                                                                
089800     EJECT                                                                
089900 IMS-GHU-KREE-ANM SECTION.                                                
090000                                                                          
090100     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE '    ' TO GODK-STATUSKODER                                      
090400     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-WLKREE01 SSA1                 
090500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     EJECT                                                                
090900 IMS-REPL-KREE-ANM SECTION.                                               
091000                                                                          
091100     MOVE '  ' TO GODK-STATUSKODER                                        
091200     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-WLKREE01                     
091300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
091600     EJECT                                                                
091700 IMS-GHU-KREE-LEV SECTION.                                                
091800                                                                          
091900     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
092000          DELIMITED BY SIZE INTO SSA1                                     
092100     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
092200          DELIMITED BY SIZE INTO SSA2                                     
092300     MOVE '    ' TO GODK-STATUSKODER                                      
092400     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-WLKREE11 SSA1 SSA2            
092500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     SKIP3                                                                
092900 IMS-REPL-KREE-LEV SECTION.                                               
093000                                                                          
093100     MOVE '  ' TO GODK-STATUSKODER                                        
093200     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-WLKREE11                     
093300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     .                                                                    
093600     EJECT                                                                
093700 IMS-GU-ARTC-ART SECTION.                                                 
093800                                                                          
093900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
094000          DELIMITED BY SIZE INTO SSA1                                     
094100     MOVE '    ' TO GODK-STATUSKODER                                      
094200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
094300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
094400     PERFORM IMS-STATUSKONTROLL                                           
094500     .                                                                    
094600     EJECT                                                                
094700 IMS-GNP-ARTC-CLAG SECTION.                                               
094800                                                                          
094900     MOVE 'WLARTC11 ' TO SSA1                                             
095000     MOVE '    ' TO GODK-STATUSKODER                                      
095100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
095200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     SKIP3                                                                
095600 IMS-GHNP-ARTC-CLAG SECTION.                                              
095700                                                                          
095800     MOVE 'WLARTC11 ' TO SSA1                                             
095900     MOVE '    ' TO GODK-STATUSKODER                                      
096000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC11 SSA1                
096100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
096200     PERFORM IMS-STATUSKONTROLL                                           
096300     .                                                                    
096400     SKIP3                                                                
096500 IMS-REPL-ARTC SECTION.                                                   
096600                                                                          
096700     MOVE '  ' TO GODK-STATUSKODER                                        
096800     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
096900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     EJECT                                                                
097300 IMS-GET-HOLD-SALDON-WDK711 SECTION.                                      
097400*     DISPLAY '**** IMS-GET-HOLD-SALDON-WDK711'                           
097500                                                                          
097600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
097700            DELIMITED BY SIZE INTO SSA1                                   
097800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
097900            DELIMITED BY SIZE INTO SSA2                                   
098000                                                                          
098100     MOVE '  GE' TO GODK-STATUSKODER                                      
098200     CALL CBLTDLI USING                                                   
098300           GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2                           
098400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
098500     PERFORM IMS-STATUSKONTROLL                                           
098600     .                                                                    
098700     EJECT                                                                
098800 IMS-REPL-SALDON-WDK711 SECTION.                                          
098900*     DISPLAY '***** IMS-REPL-SALDON-WDK711 '                             
099000                                                                          
099100     MOVE '  ' TO GODK-STATUSKODER                                        
099200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
099300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
099400     PERFORM IMS-STATUSKONTROLL                                           
099500     .                                                                    
099600     EJECT                                                                
099601 IMS-GU-WDK711 SECTION.                                                   
099602                                                                          
099603     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
099604             DELIMITED BY SIZE INTO SSA1                                  
099605     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
099606             DELIMITED BY SIZE INTO SSA2                                  
099607     MOVE '  ' TO GODK-STATUSKODER                                        
099608     CALL  CBLTDLI  USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
099609     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
099610     PERFORM IMS-STATUSKONTROLL                                           
099611     .                                                                    
099612 IMS-GHNP-WDK728-LAST SECTION.                                            
099620                                                                          
099630     STRING 'WDK728  *L(DAINLEV < ' W-DAINLEV ')'                         
099640          DELIMITED BY SIZE INTO SSA1                                     
099650     MOVE '  GE' TO GODK-STATUSKODER                                      
099660     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728 SSA1                  
099670     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
099680     PERFORM IMS-STATUSKONTROLL                                           
099690     .                                                                    
099691     EJECT                                                                
099692 IMS-REPL-WDK728 SECTION.                                                 
099693                                                                          
099694     MOVE '  ' TO GODK-STATUSKODER                                        
099695     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
099696     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
099697     PERFORM IMS-STATUSKONTROLL                                           
099698     .                                                                    
099699     EJECT                                                                
099700 IMS-GNP-INVA-INVA11 SECTION.                                             
099800                                                                          
099900     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
100000          DELIMITED BY SIZE INTO SSA1                                     
100100     STRING  'WDH111  (WDH111KY>=' W-WDH111KY-MIN                         
100200                     '&WDH111KY<=' W-WDH111KY-MAX ')'                     
100300              DELIMITED BY SIZE INTO SSA2                                 
100400     MOVE '  GE' TO GODK-STATUSKODER                                      
100500     CALL CBLTDLI USING GU INVA-PCB WDH111 SSA1 SSA2                      
100600     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
100700     PERFORM IMS-STATUSKONTROLL                                           
100800     .                                                                    
100900     EJECT                                                                
101000 IMS-ISRT-WDH101 SECTION.                                                 
101100                                                                          
101200     MOVE 'WDH101 ' TO SSA1                                               
101300     MOVE '  II' TO GODK-STATUSKODER                                      
101400     CALL CBLTDLI USING ISRT INVA-PCB WDH101 SSA1                         
101500     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
101600     PERFORM IMS-STATUSKONTROLL                                           
101700     .                                                                    
101800     EJECT                                                                
101900 IMS-ISRT-WDH111 SECTION.                                                 
102000                                                                          
102100     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
102200          DELIMITED BY SIZE INTO SSA1                                     
102300     MOVE 'WDH111 ' TO SSA2                                               
102400     MOVE '  II' TO GODK-STATUSKODER                                      
102500     CALL CBLTDLI USING ISRT INVA-PCB WDH111 SSA1 SSA2                    
102600     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     .                                                                    
102900     EJECT                                                                
103000 IMS-ISRT-WDH121 SECTION.                                                 
103100                                                                          
103200     MOVE 'WDH121 ' TO SSA1                                               
103300     MOVE '  II' TO GODK-STATUSKODER                                      
103400     CALL CBLTDLI USING ISRT INVA-PCB WDH121 SSA1                         
103500     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
103600     PERFORM IMS-STATUSKONTROLL                                           
103700     .                                                                    
103800     EJECT                                                                
103900 IMS-LAS-ATERSTART SECTION.                                               
104000                                                                          
104100     STRING 'WL412101(WDGXKEY  =' W-IDHTYP-X ')'                          
104200                    DELIMITED BY SIZE INTO SSA1                           
104300     MOVE 'WL412111 '    TO SSA2                                          
104400     MOVE '  '           TO GODK-STATUSKODER                              
104500     CALL CBLTDLI USING GHU 4121-PCB DLI-IO-AREA-2 SSA1 SSA2              
104600     MOVE 4121-STATUS-CODE TO STATUS-WS                                   
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     .                                                                    
104900     SKIP2                                                                
105000 IMS-REPL-ATERSTART SECTION.                                              
105100                                                                          
105200     MOVE '  '             TO GODK-STATUSKODER                            
105300     CALL CBLTDLI USING REPL 4121-PCB DLI-IO-AREA-2                       
105400     MOVE 4121-STATUS-CODE TO STATUS-WS                                   
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     .                                                                    
105700     EJECT                                                                
105800 IMS-RESTART SECTION.                                                     
105900                                                                          
106000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
106100     MOVE '  ' TO GODK-STATUSKODER                                        
106200     CALL CBLTDLI USING XRST MSG-PCB                                      
106300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
106400                        CHKP-AREA-LENGTH CHKP-AREA                        
106500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106600     PERFORM IMS-STATUSKONTROLL                                           
106700     .                                                                    
106800     EJECT                                                                
106900 IMS-CHECKPOINT SECTION.                                                  
107000     SKIP2                                                                
107100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
107200     MOVE '  XD' TO GODK-STATUSKODER                                      
107300     CALL CBLTDLI USING CHKP MSG-PCB                                      
107400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
107500                        CHKP-AREA-LENGTH CHKP-AREA                        
107600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107700     PERFORM IMS-STATUSKONTROLL                                           
107800                                                                          
107900     IF IMS-EJ-OK                                                         
108000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
108100       DISPLAY FELTEXT                                                    
108200       CALL FELLOG                                                        
108300     END-IF                                                               
108400     .                                                                    
108500     EJECT                                                                
108600 IMS-ISRT-WDL901 SECTION.                                                 
108700                                                                          
108800     MOVE 'WLLOGA01 ' TO SSA1                                             
108900     MOVE '  II' TO GODK-STATUSKODER                                      
109000     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
109100     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
109200     PERFORM IMS-STATUSKONTROLL                                           
109300     .                                                                    
109400     EJECT                                                                
109500 IMS-GHU-WDGX4103 SECTION.                                                
109600                                                                          
109700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
109800          DELIMITED BY SIZE INTO SSA1                                     
109900     MOVE '  GE' TO GODK-STATUSKODER                                      
110000     CALL CBLTDLI USING GHU 4103-PCB DLI-IO-WDGX4103 SSA1                 
110100     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
110200     PERFORM IMS-STATUSKONTROLL                                           
110300     .                                                                    
110400     SKIP3                                                                
110500 IMS-DLET-WDGX4103 SECTION.                                               
110600                                                                          
110700     MOVE '  ' TO GODK-STATUSKODER                                        
110800     CALL CBLTDLI USING DLET 4103-PCB DLI-IO-WDGX4103                     
110900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     .                                                                    
111200     EJECT                                                                
111210 IMS-GU-WDB201    SECTION.                                                
111220                                                                          
111230     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
111240          DELIMITED BY SIZE INTO SSA1                                     
111270     MOVE '    ' TO GODK-STATUSKODER                                      
111280     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
111290     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
111291     PERFORM IMS-STATUSKONTROLL                                           
111292     .                                                                    
111293     SKIP3                                                                
111300 IMS-STATUSKONTROLL SECTION.                                              
111400     SKIP2                                                                
111500     SET STATUS-IX TO 1                                                   
111600     SEARCH GODK-STATUS                                                   
111700       AT END                                                             
111800         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
111900         DISPLAY FELTEXT                                                  
112000         CALL FELLOG                                                      
112100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
112200         CONTINUE                                                         
112300     END-SEARCH                                                           
112400     .                                                                    
