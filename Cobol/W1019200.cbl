000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1019200.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   APRIL 1988.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                UPPDATERINGSPGM TILL 1116.                               
001100*                                                                         
001200*                OBS  VID NYUPPLÄGG GÖRS SAMMA SAK I                      
001300*                     SEKTION S02-UPPDATERA-ARTREG OCH                    
001400*                     SEKTION EB-KOPIERA-ARTIKEL                          
001500*                                                                         
001600*    ÄNDRINGAR:                                                           
001700*        2003-01-09 UPPD. BENA-BEN-FLAENDR VID FÖRÄNDRING PÅ WDD3         
001800*                                                           /CE           
001900*        2005-06-23 TILLÄGG AV 9 NYA SPRÅK (WDD311) VID NYUPPLÄGG.        
002000*                                                           /CE           
002100*        2005-10-11                                                       
002200*        KOLLA ATT KDPRODSL = 11-29 INNAN WDD3-BEN-FLAENDR = JA           
002300*                                                           /CE           
002400*        2016-12-15 LAGT TILL TISTODAT-LARM                 /IS           
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W1T192                                              
002800*        MID:         W1I11601                                            
002900*                     W1O11601                                            
003000*    UTDATA.                                                              
003100*        MOD:         W1O11601                                            
003200*                                                                         
003300*    SUBPROGRAM:      CBLTDLI                                             
003400*                     FELLOG                                              
003500*                     W009KSIF                                            
003600*                     WDATKONV                                            
003700*                     W009VADD                                            
003800*                     RW009REDU                                           
003900*                     WKPSKONV                                            
004000*                     W009CIA                                             
004100     EJECT                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     SKIP3                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  PROGRAM-NAMN              PIC X(08)   VALUE 'W1019200'.              
005000 77  JA                        PIC X       VALUE 'J'.                     
005100 77  NEJ                       PIC X       VALUE 'N'.                     
005200                                                                          
005300 77  WS-W009REDU-IN            PIC X(30)   VALUE SPACE.                   
005400 77  WS-W009REDU-UT            PIC X(30)   VALUE SPACE.                   
005500 77  WS-BEART                  PIC X(25)   VALUE SPACE.                   
005600 77  WS-BEART-SVE              PIC X(25)   VALUE SPACE.                   
005700 77  WS-IDSKYLT                PIC X(03)   VALUE SPACE.                   
005800 77  WS-FLRSBEART              PIC X(01)   VALUE SPACE.                   
005900                                                                          
006000 77  SPAR-PRARTBTO-EXP      PIC S9(7)V9(2) COMP-3 VALUE ZERO.             
006100 77  WS-BELEV                  PIC X(30)   VALUE SPACE.                   
006300 77  WS-FLGAMART               PIC X       VALUE SPACE.                   
006400 77  WS-FLLSRDEL               PIC X       VALUE SPACE.                   
006500 77  WS-FLBYTES                PIC X       VALUE SPACE.                   
006600 77  WS-IDLEVNR                PIC X(5)    VALUE SPACE.                   
006700 77  WS-IDAO                   PIC X(10)   VALUE SPACE.                   
006800 77  WS-IDBERED                PIC 9(2)    VALUE ZERO.                    
006900 77  WS-IDPROJ                 PIC X(4)    VALUE SPACE.                   
007000 77  WS-IDPROJK                PIC X(4)    VALUE SPACE.                   
007100 77  WS-IDPROJUP               PIC X(8)    VALUE SPACE.                   
007200 77  WS-IDRITN                 PIC X(10)   VALUE SPACE.                   
007300 77  WS-IDFKNGRP               PIC 9(4)    VALUE ZERO.                    
007400 77  WS-KDPRODSL               PIC 9(2)    VALUE ZERO.                    
007500 77  WS-KDSORT                 PIC X(2)    VALUE SPACE.                   
007600 77  WS-KDUART                 PIC X       VALUE SPACE.                   
007700 77  WS-KDYTBEH                PIC 9(2)    VALUE ZERO.                    
007800 77  WS-KDBPSR                 PIC 9(1)    VALUE ZERO.                    
007900 77  WS-KDFARLIG               PIC 9(1)    VALUE ZERO.                    
008000 77  WS-TEARTNOT-2             PIC X(40)   VALUE SPACE.                   
008100 77  WS-TEARTNOT-4             PIC X(40)   VALUE SPACE.                   
008200 77  WS-TEARTNOT-7             PIC X(40)   VALUE SPACE.                   
008300 77  WS-KDAGE                  PIC X       VALUE SPACE.                   
008400 77  WS-KDPSLLOC               PIC 9(2)    VALUE ZERO.                    
008500 77  SW-NYPON-ART              PIC X       VALUE 'N'.                     
008700 77  SAMMA-BEN                 PIC X       VALUE 'N'.                     
008800 77  MOD-IX                    PIC 9(1)    VALUE ZERO.                    
008900 77  MAX-IX                    PIC S9(3)   VALUE +3.                      
009000 77  MAX-IX-PLUS-1             PIC S9(3)   VALUE +4.                      
009100 77  MAX-PROJK-IX              PIC S9(3)   VALUE +75.                     
009200 77  MAX-PROJK-IX-PLUS-1       PIC S9(3)   VALUE +76.                     
009300 77  SPIND                     PIC S9(9)   VALUE +0   COMP SYNC.          
009400 77  IX                        PIC S9(9)   VALUE +0   COMP SYNC.          
009500 77  PROJK-IX                  PIC S9(9)   VALUE +0   COMP SYNC.          
009600 77  TAB-IX                    PIC S9(9)   VALUE +0   COMP-3.             
009700 77  MAX-MOD-LAENGD            PIC S9(4)   VALUE +690 COMP SYNC.          
009800     SKIP3                                                                
009900                                                                          
010000 01  WS-TEHOMONYM.                                                        
010100     03  RS-BM-NAMN              PIC X(7).                                
010200     03  FILLER                  PIC X(53).                               
010300     SKIP3                                                                
010400 01  IDSKYLT-TABELL.                                                      
010500     03 FILLER                   PIC X(3)    VALUE 'CZ '.                 
010600     03 FILLER                   PIC X(3)    VALUE 'D  '.                 
010700     03 FILLER                   PIC X(3)    VALUE 'DK '.                 
010800     03 FILLER                   PIC X(3)    VALUE 'E  '.                 
010900     03 FILLER                   PIC X(3)    VALUE 'F  '.                 
011000     03 FILLER                   PIC X(3)    VALUE 'GB '.                 
011100     03 FILLER                   PIC X(3)    VALUE 'GR '.                 
011200     03 FILLER                   PIC X(3)    VALUE 'H  '.                 
011300     03 FILLER                   PIC X(3)    VALUE 'I  '.                 
011400     03 FILLER                   PIC X(3)    VALUE 'IR '.                 
011500     03 FILLER                   PIC X(3)    VALUE 'J  '.                 
011600     03 FILLER                   PIC X(3)    VALUE 'KOR'.                 
011700     03 FILLER                   PIC X(3)    VALUE 'MAL'.                 
011800     03 FILLER                   PIC X(3)    VALUE 'NL '.                 
011900     03 FILLER                   PIC X(3)    VALUE 'P  '.                 
012000     03 FILLER                   PIC X(3)    VALUE 'PL '.                 
012100     03 FILLER                   PIC X(3)    VALUE 'RC '.                 
012200     03 FILLER                   PIC X(3)    VALUE 'RCN'.                 
012300     03 FILLER                   PIC X(3)    VALUE 'RO '.                 
012400     03 FILLER                   PIC X(3)    VALUE 'RUS'.                 
012500     03 FILLER                   PIC X(3)    VALUE 'S  '.                 
012600     03 FILLER                   PIC X(3)    VALUE 'SF '.                 
012700     03 FILLER                   PIC X(3)    VALUE 'T  '.                 
012800     03 FILLER                   PIC X(3)    VALUE 'TR '.                 
012900     03 FILLER                   PIC X(3)    VALUE 'USA'.                 
013000     03 FILLER                   PIC X(3)    VALUE 'YU '.                 
013100 01  TAB  REDEFINES  IDSKYLT-TABELL.                                      
013200     03  FILLER OCCURS 26.                                                
013300         05  TAB-IDSKYLT         PIC X(3).                                
013400 01  MAX-TAB-IDSKYLT             PIC S9(3)   VALUE +26.                   
013500*                                                                         
022700*01  -COPY WWPRODSL                                                       
022800*                                                                         
022900     EJECT                                                                
023000                                                                          
023100 01  WS-LV-IDARTNR                    PIC X(27) VALUE SPACE.              
023200 01  WS-IDARTNR                       PIC X(9) VALUE SPACE.               
023300 01  IDARTNR-WS REDEFINES WS-IDARTNR  PIC 9(9).                           
023400     SKIP2                                                                
023500 01  WS-NY-IDARTNR                    PIC X(9) VALUE SPACE.               
023600 01  IDARTNR-NY-WS REDEFINES WS-NY-IDARTNR PIC 9(9).                      
023700     SKIP2                                                                
023800 01  WS-FLERS                    PIC X        VALUE SPACE.                
023900 01  WS-KDERS-UTG                PIC S9(3) COMP-3 VALUE ZERO.             
024000     EJECT                                                                
024100 01  KOPIERING                   PIC X        VALUE 'N'.                  
024200 01  WS-ANSK-FINNS               PIC X        VALUE 'N'.                  
024300     SKIP3                                                                
024400 01  WS-TIFINLV                PIC 9(5)    VALUE ZERO.                    
024500 01  FILLER REDEFINES WS-TIFINLV.                                         
024600     03  WS-AAR                PIC 9(2).                                  
024700     03  WS-VECKA              PIC 9(2).                                  
024800     03  WS-DAG                PIC 9(1).                                  
024900     SKIP2                                                                
025000 01  XX-TIFINLV                PIC X(5)    VALUE SPACE.                   
025100 01  FILLER REDEFINES XX-TIFINLV.                                         
025200     03  XX-AAR                PIC X(2).                                  
025300     03  XX-VECKA              PIC X(2).                                  
025400     03  XX-DAG                PIC X(1).                                  
025500     SKIP2                                                                
025600 01  WS-TISOP                  PIC 9(5)    VALUE ZERO.                    
025700 01  FILLER REDEFINES WS-TISOP.                                           
025800     03  WS-AAR-SOP            PIC 9(2).                                  
025900     03  WS-VECKA-SOP          PIC 9(2).                                  
026000     03  WS-DAG-SOP            PIC 9(1).                                  
026100     SKIP2                                                                
026200 01  XX-TISOP                  PIC X(5)    VALUE SPACE.                   
026300 01  FILLER REDEFINES XX-TISOP.                                           
026400     03  XX-AAR-SOP            PIC X(2).                                  
026500     03  XX-VECKA-SOP          PIC X(2).                                  
026600     03  XX-DAG-SOP            PIC X(1).                                  
026700     SKIP2                                                                
026800 01  INMATAD-IDPROENH.                                                    
026900     03  FILLER OCCURS 3.                                                 
027000         05  WS-IDPROENH         PIC X(8).                                
027100     SKIP2                                                                
027200 01  INMATAD-IDKAT.                                                       
027300     03  FILLER OCCURS 3.                                                 
027400         05  WS-IDKAT            PIC X(5).                                
027500     SKIP2                                                                
027600 01  W009KSIF-FALT.                                                       
027700     03  RESK-IDART              PIC 9(9).                                
027800     03  RESK-9-POS              PIC 9        VALUE 9.                    
027900     03  RESK-W009KSIFR          PIC 9.                                   
028000     SKIP3                                                                
028100 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
028200 01  WS-TIBORT                   PIC S9(7) COMP-3 VALUE ZERO.             
028300     SKIP2                                                                
028400 01  VECKOR.                                                              
028500     03  AAVVD                   PIC 9(5).                                
028600     03  FILLER REDEFINES AAVVD.                                          
028700         05  AAVV                PIC 9(4).                                
028800         05  D                   PIC 9(1).                                
028900     SKIP2                                                                
029000                                                                          
029100 01  SPAR-TIFINLV-AAVVD        PIC 9(05)  VALUE ZERO.                     
029200                                                                          
029300 01  SPAR-TIFINLV-AAVV.                                                   
029400     10  SPAR-TIFINLV-AA       PIC 9(02)  VALUE ZERO.                     
029500     10  SPAR-TIFINLV-VV       PIC 9(02)  VALUE ZERO.                     
029600 01  SPAR-TIFINLV-AAVV-R  REDEFINES  SPAR-TIFINLV-AAVV                    
029700                               PIC 9(04).                                 
029800                                                                          
029900                                                                          
030000 01  SPAR-DAGENS-AAVV.                                                    
030100     03  SPAR-DAGENS-AA           PIC 9(02)  VALUE ZERO.                  
030200     03  SPAR-DAGENS-VV           PIC 9(02)  VALUE ZERO.                  
030300 01  SPAR-DAGENS-AAVV-R  REDEFINES  SPAR-DAGENS-AAVV                      
030400                                  PIC 9(04).                              
030500                                                                          
030600 01  WS-YYWWD                    PIC 9(5).                                
030700 01  FILLER REDEFINES WS-YYWWD.                                           
030800     03  WS-YY                   PIC 9(2).                                
030900     03  WS-WW                   PIC 9(2).                                
031000     03  WS-D                    PIC 9(1).                                
031100 01  WS-TID                      PIC S9(9).                               
031200 01  WS-DAREGDAT                 PIC 9(8).                                
031300     SKIP2                                                                
031400**** VARIABLER TILL W009VADD****                                          
031500 01  W009VADD-DATUM              PIC S9(5)    COMP-3.                     
031600 01  W009VADD-ANTAL              PIC S9(3)    COMP-3.                     
031700     SKIP3                                                                
031800 01  ABEND-KODER.                                                         
031900     03  RKOD-ABEND-MED-DUMP  PIC S9(4) COMP SYNC VALUE +1000.            
032000     03  RKOD-ABEND-UTAN-DUMP PIC S9(4) COMP SYNC VALUE   +16.            
032100     SKIP3                                                                
032200 01  DYNAMISKA-SUBPROGRAM.                                                
032300     03  W009KSIF                PIC X(8)     VALUE 'W009KSIF'.           
032400     03  W009VADD                PIC X(8)     VALUE 'W009VADD'.           
032500     03  WDATKONV                PIC X(8)     VALUE 'WDATKONV'.           
032600     03  WREVERSE                PIC X(8)     VALUE 'WREVERSE'.           
032700     03  W009REDU                PIC X(8)     VALUE 'W009REDU'.           
032800     03  CBLTDLI                 PIC X(8)     VALUE 'CBLTDLI '.           
032900     03  FELLOG                  PIC X(8)     VALUE 'FELLOG  '.           
033000     03  WKPSKONV                PIC X(8)     VALUE 'WKPSKONV'.           
033100     03  W009CIA                 PIC X(8)     VALUE 'W009CIA '.           
033200     03  ABEND                   PIC X(8)     VALUE 'ABEND   '.           
033210     03  W100LPC                 PIC X(8)     VALUE 'W100LPC'.            
033220     03  W221SEGM                PIC X(8)     VALUE 'W221SEGM'.           
033300     EJECT                                                                
033400*   -COPY W009CIA                                                         
033500     EJECT                                                                
033600*   -COPY WDATAREA                                                        
033700     EJECT                                                                
033800*   -COPY WREVAREA                                                        
033900     EJECT                                                                
034000*   -COPY WKPSAREA                                                        
034100     EJECT                                                                
034200*01 -COPY W10111                                                          
034300     EJECT                                                                
034400*01 -COPY W1011102                                                        
034500     EJECT                                                                
034600*01 -COPY WWBYT03                                                         
034700     EJECT                                                                
034800*01  AREA -COPY W092W001           -PRE W092-                             
034900     EJECT                                                                
035000*   -COPY W200PRNC                                                        
035100     EJECT                                                                
035120 01  FILLER                      PIC X(16)   VALUE 'W100LPC-AREA'.        
035130*01  LPC-AREA  -COPY W100LPC                                              
035140     EJECT                                                                
035200 01  NYCKLAR-TILL-DLI.                                                    
035300   03  W-IDARTNR-X.                                                       
035400     05  W-IDARTNR               PIC S9(9)   COMP-3  VALUE ZERO.          
035500   03  W-IDARTNR-SATS-X.                                                  
035600     05  W-IDARTNR-SATS          PIC S9(9)   COMP-3  VALUE ZERO.          
035700   03  W-IDLEVNR-X.                                                       
035800     05  W-IDLEVNR-LEVA          PIC X(5)    VALUE SPACE.                 
035900   03  W-KDNOTTYP-X.                                                      
036000     05  W-KDNOTTYP              PIC S9      COMP-3  VALUE ZERO.          
036100   03  W-IDLOGLOP-X.                                                      
036200     05  W-IDLOGLOP              PIC S9      COMP-3  VALUE ZERO.          
036600   03  W-1131-KEY-X.                                                      
036700     05  FILLER                  PIC X(4)    VALUE '1131'.                
036800     05  W-KDPRODSL-1131         PIC S9(3)   COMP-3 VALUE ZERO.           
036900     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
037000   03  W-1132-KEY-X.                                                      
037100     05  W-IDPROJK               PIC X(4)    VALUE LOW-VALUE.             
037200     05  W-IDPROJOBJ             PIC X(4)    VALUE LOW-VALUE.             
037300     05  W-IDPROJ                PIC X(4)    VALUE LOW-VALUE.             
037400     05  FILLER                  PIC X(3)    VALUE LOW-VALUE.             
037500   03  W-1137-KEY-X.                                                      
037600     05  FILLER                  PIC X(4)    VALUE '1137'.                
037700     05  W-KDPRODSL-1137         PIC S9(3)   COMP-3 VALUE ZERO.           
037800     05  FILLER                  PIC X(24)   VALUE LOW-VALUE.             
037900   03  W-1139-KEY-X.                                                      
038000     05  FILLER                  PIC X(4)    VALUE '1139'.                
038100     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
038200   03  W-2227-KEY-X.                                                      
038300     05  FILLER                  PIC X(4)    VALUE '2227'.                
038400     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
038500   03  W-9101-KEY-X.                                                      
038600     05  FILLER                  PIC X(4)    VALUE '9101'.                
038700     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
038800   03  W-IDSKYLT-X.                                                       
038900     05  W-IDSKYLT               PIC X(03)   VALUE SPACE.                 
039000   03  W-BEART-X.                                                         
039100     05  W-BEART                 PIC X(25)   VALUE SPACE.                 
039200   03  W-IDBENNR-X.                                                       
039300     05  W-IDBENNR               PIC S9(7)   COMP-3 VALUE ZERO.           
039400   03  W-1207-KEY-X.                                                      
039500     05  FILLER                  PIC X(04)   VALUE '1207'.                
039600     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
039700   03  W-WDJ1CSEQ-X.                                                      
039800     05  W-IDLEVNR-S             PIC X(5)    VALUE SPACE.                 
039900     05  W-BELEVART-S            PIC X(30)   VALUE SPACE.                 
040000     05  W-IDARTNR-S             PIC S9(9)   COMP-3 VALUE ZERO.           
040100   03  W-WDJ111KY-X.                                                      
040200     05  W-KDSTRRAD              PIC X       VALUE SPACE.                 
040300     05  W-IDRADNR               PIC S9(5)   COMP-3 VALUE ZERO.           
040400     EJECT                                                                
040500   03  W-WDGX2263-X.                                                      
040600     05  W-IDHTYP-2263           PIC X(4)    VALUE '2263'.                
040700     05  W-FILLER                PIC X(26)   VALUE LOW-VALUE.             
040800   03  W-TISOP-O-X.                                                       
040900     05  W-TISOP-2264-O          PIC S9(5)   COMP-3.                      
041000   03  W-W2266KY-MIN-O-X.                                                 
041100     05  W-IDARTNR-2266-O-MIN    PIC S9(9)   COMP-3.                      
041200     05  W-IDDC-2266-O-MIN       PIC X(2)    VALUE LOW-VALUE.             
041300   03  W-W2266KY-MAX-O-X.                                                 
041400     05  W-IDARTNR-2266-O-MAX    PIC S9(9)   COMP-3.                      
041500     05  W-IDDC-2266-O-MAX       PIC X(2)    VALUE HIGH-VALUE.            
041600******************************************************************        
041700*                                                                         
041800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
041900*                                                                         
042000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
042100 01    W-PROG-TO-PROG-SW.                                                 
042200       03 M-SW-LL                PIC S9(4) VALUE +290 COMP SYNC.          
042300       03 M-SW-Z1-Z2             PIC  X(2) VALUE LOW-VALUE.               
042400       03 M-SW-KDTRANS           PIC  X(8) VALUE 'W0T107X'.               
042500       03 M-SW-IDTRANS           PIC  X(4) VALUE '1192'.                  
042600       03 M-SW-KDMFSTYP          PIC  X(1) VALUE '1'.                     
042700                                                                          
042800       03  MID -COPY W0I10701 -PRE PROG-                                  
042900     EJECT                                                                
043000 01    INAREA.                                                            
043100*  03  W1I11601 -COPY W1I11601                                            
043200     EJECT                                                                
043300*  03  W1O11601 -COPY W1O11601                                            
043400     EJECT                                                                
043500*01    -COPY WMSGAREA                                                     
043600     EJECT                                                                
043700*  03    MOD -COPY W1O11601  -PRE MOD-  -RED MSG-AREA.                    
043800     EJECT                                                                
043900*01    -COPY WMFSAREA                                                     
044000     EJECT                                                                
044010*    --- PARAMETRAR TILL W221SEGM                                         
044020*                                                                         
044030*01  -COPY W221SEGM                                                       
044040     EJECT                                                                
044050                                                                          
044100******************************************************************        
044200*                                                                         
044300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
044400*                                                                         
044500 01    IMS-WS.                                                            
044600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
044700     SKIP3                                                                
044800*                        **** STATUS-KOD FRÅN IMS                         
044900   03    STATUS-WS               PIC XX.                                  
045000     88    SEGMENT-FINNS                     VALUE '  '.                  
045100     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
045200     88    BASEN-SLUT                        VALUE 'GB'.                  
045300     SKIP3                                                                
045400   03    GODK-STATUSKODER.                                                
045500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
045600     SKIP3                                                                
045700 01    SSA1                      PIC X(96).                               
045800 01    SSA2                      PIC X(64).                               
045900 01    SSA3                      PIC X(64).                               
046000     EJECT                                                                
046100*                            IMS FUNKTIONSKODER                           
046200*01    -COPY W0003                                                        
046300     EJECT                                                                
046400*                            DLI INPUT-OUTPUT AREA                        
046500 01    DLI-IO-AREA.                                                       
046600   03    IO-AREA                 PIC X(1500) VALUE SPACE.                 
046700     SKIP3                                                                
046800*  03  WDK601 -COPY WDK601                   -RED IO-AREA.                
046900     EJECT                                                                
047000*  03  WDK611 -COPY WDK611                   -RED IO-AREA.                
047100     EJECT                                                                
047200*  03  WDK625 -COPY WDK625                   -RED IO-AREA.                
047300     EJECT                                                                
047400*  03  WLZZAC01 -COPY WDGZ01  -PRE ZZAC-     -RED IO-AREA.                
047500     EJECT                                                                
047600*  03  WLXXAQ11 -COPY WDGX1132  -PRE XXAQ-   -RED IO-AREA.                
047700     EJECT                                                                
047800*  03  WLXXAT11 -COPY WDGX1138  -PRE XXAT-   -RED IO-AREA.                
047900     EJECT                                                                
048000*  03  WLXXBW11 -COPY WDGX2228  -PRE XXBW-   -RED IO-AREA.                
048100     EJECT                                                                
048200*  03  WLBENA01 -COPY WDD301  -PRE BENA-     -RED IO-AREA.                
048300     EJECT                                                                
048400*  03  WLBENA11 -COPY WDD311  -PRE BENA-     -RED IO-AREA.                
048500     EJECT                                                                
048600*  03  WLBENA12 -COPY WDD312  -PRE BENA-     -RED IO-AREA.                
048700     EJECT                                                                
048800*  03  WLBENA13 -COPY WDD313  -PRE BENA-     -RED IO-AREA.                
048900     EJECT                                                                
049200*  03  WLXXID11 -COPY WDG39101   -PRE XXID-  -RED IO-AREA.                
049300     EJECT                                                                
049400 01    DLI-IO-AREA-2.                                                     
049500   03    IO-AREA-2               PIC X(600)  VALUE SPACE.                 
049600     SKIP3                                                                
049700*  03  WLARTG01 -COPY WDD201  -PRE ARTG01-   -RED IO-AREA-2.              
049800     EJECT                                                                
049900 01    DLI-IO-AREA-3.                                                     
050000   03    IO-AREA-3               PIC X(516)  VALUE SPACE.                 
050100     SKIP3                                                                
050200*  03  WLSATB01 -COPY WDJ101   -PRE SATB-    -RED IO-AREA-3.              
050300*  03  WLSATB11 -COPY WDJ111   -PRE SATB-    -RED IO-AREA-3.              
050400     EJECT                                                                
050500   03  WDJ1CSEQ REDEFINES IO-AREA-3.                                      
050600*      05  WLSATE   -COPY WDJ111  -PRE SATE-                              
050700*      05  WLSATE   -COPY WDJ101  -PRE SATE-                              
050800     EJECT                                                                
050810 01  FILLER         PIC X(20) VALUE 'DLI-IO-AREA-XXAI'.                   
050820 01  DLI-IO-AREA-XXAI.                                                    
050821*  03  WLXXAI11 -COPY WDGX1208   -PRE XXAI-                               
050822     EJECT                                                                
050900 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2264-O'.                  
051000 01  DLI-IO-WDGX2264-O.                                                   
051100*    03  -COPY WDGX2264       -PRE OLD-                                   
051200     EJECT                                                                
051300 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2266-O'.                  
051400 01  DLI-IO-WDGX2266-O.                                                   
051500*    03  -COPY WDGX2266      -PRE OLD-                                    
051600     EJECT                                                                
051700 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2264-N'.                  
051800 01  DLI-IO-WDGX2264-N.                                                   
051900*    03  -COPY WDGX2264       -PRE NEW-                                   
052000     EJECT                                                                
052100 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2266-N'.                  
052200 01  DLI-IO-WDGX2266-N.                                                   
052300*    03  -COPY WDGX2266      -PRE NEW-                                    
052400     EJECT                                                                
052500 LINKAGE SECTION.                                                         
052600*01    -COPY W0009     -PRE MSG-                                          
052700                                                                          
052800*01    -COPY W0009     -PRE ALT-                                          
052900     EJECT                                                                
053000*01    -COPY W0008     -PRE ARTC-                                         
053100     05  FILLER                  PIC X.                                   
053200     EJECT                                                                
053300*01    -COPY W0008     -PRE ZZAC-                                         
053400     05  FILLER                  PIC X.                                   
053500                                                                          
053600*01    -COPY W0008     -PRE ARTG-                                         
053700     05  FILLER                  PIC X.                                   
053800     EJECT                                                                
053900*01    -COPY W0008     -PRE XXAQ-                                         
054000     05  FILLER                  PIC X.                                   
054100                                                                          
054200*01    -COPY W0008     -PRE XXAT-                                         
054300     05  FILLER                  PIC X.                                   
054400     EJECT                                                                
054500*01    -COPY W0008     -PRE XXBW-                                         
054600     05  FILLER                  PIC X.                                   
054700                                                                          
054800*01    -COPY W0008     -PRE BENA-                                         
054900     05  FILLER                  PIC X.                                   
055000     EJECT                                                                
055100*01    -COPY W0008     -PRE BENB-                                         
055200     05  FILLER                  PIC X.                                   
055300                                                                          
055400*01    -COPY W0008     -PRE BENC-                                         
055500     05  FILLER                  PIC X.                                   
055600     EJECT                                                                
055700*01    -COPY W0008     -PRE XXAI-                                         
055800     05  FILLER                  PIC X.                                   
055900                                                                          
056000*01    -COPY W0008     -PRE SATB-                                         
056100     05  FILLER                  PIC X.                                   
056200     EJECT                                                                
056300*01    -COPY W0008     -PRE SATE-                                         
056400     05  FILLER                  PIC X.                                   
056500                                                                          
056600*01    -COPY W0008     -PRE XXID-                                         
056700     05  FILLER                  PIC X.                                   
056800     EJECT                                                                
056900*01    -COPY W0008     -PRE WDR2-O-                                       
057000     05  FILLER                  PIC X.                                   
057100     EJECT                                                                
057200*01    -COPY W0008     -PRE WDR2-N-                                       
057300     05  FILLER                  PIC X.                                   
057400     EJECT                                                                
057410 01  SEGM-WDB6-PCB                 PIC X.                                 
057420 01  SEGM-WDL7-PCB                 PIC X.                                 
057430 01  SEGM-WDL8-PCB                 PIC X.                                 
057440 01  SEGM-WDD5-PCB                 PIC X.                                 
057450     EJECT                                                                
057500 PROCEDURE DIVISION USING MSG-PCB ALT-PCB ARTC-PCB                        
057600                          ZZAC-PCB ARTG-PCB XXAQ-PCB                      
057700                          XXAT-PCB XXBW-PCB                               
057800                          BENA-PCB BENB-PCB BENC-PCB XXAI-PCB             
057900                          SATB-PCB SATE-PCB XXID-PCB                      
058000                          WDR2-O-PCB WDR2-N-PCB                           
058010                          SEGM-WDB6-PCB SEGM-WDL7-PCB                     
058020                          SEGM-WDL8-PCB SEGM-WDD5-PCB.                    
058050                                                                          
058100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ARTC-PCB                       
058200                           ZZAC-PCB ARTG-PCB XXAQ-PCB                     
058300                           XXAT-PCB XXBW-PCB                              
058400                          BENA-PCB BENB-PCB BENC-PCB XXAI-PCB             
058500                          SATB-PCB SATE-PCB XXID-PCB                      
058600                          WDR2-O-PCB WDR2-N-PCB                           
058610                          SEGM-WDB6-PCB SEGM-WDL7-PCB                     
058620                          SEGM-WDL8-PCB SEGM-WDD5-PCB.                    
058700     SKIP3                                                                
058800     PERFORM IMS-GET-MSG                                                  
058900     IF SEGMENT-FINNS                                                     
059000       PERFORM A-INIT                                                     
059100       IF MFS-IDTRANS = '1116'  OR  '111F' OR '9410'                      
059200*****************  IDTRANS 111F ÄR FÖR BMP-UPPDAT FRÅN MID-FIL            
059300*****************  111F-MIDAR ÄR FORMELLT KONTR INNAN UPPDAT.             
059400         IF WS-IDARTNR NUMERIC                                            
059500           IF IDARTNR-NY-WS = IDARTNR-WS                                  
059600             MOVE NEJ TO KOPIERING                                        
059700             PERFORM B-LAS-ARTIKEL                                        
059800             IF SEGMENT-FINNS                                             
059900                IF ART-KDERS-UTG > 0                                      
060000                   PERFORM C-VAECKNING-AV-GAMMAL-ARTIKEL                  
060100                END-IF                                                    
060200             ELSE                                                         
060300                PERFORM D-NYREGISTRERING                                  
060400             END-IF                                                       
060500             PERFORM S04-UPPDAT-CROSSIND-LISTPOST                         
060600           ELSE                                                           
060700             MOVE JA TO KOPIERING                                         
060800             PERFORM E-KOPIERA-ARTIKEL                                    
060900           END-IF                                                         
061000           PERFORM F-KOEHANTERING                                         
061100           PERFORM I-TRANS-TILL-HANDELSEREG                               
061200           PERFORM H-RENSA-INFAELT                                        
061300         END-IF                                                           
061400         MOVE WS-IDARTNR TO MOD-MOD-IDARTNR-UT                            
061500         INSPECT MOD-MOD-IDARTNR-UT REPLACING LEADING                     
061600              ZERO BY SPACE                                               
061700         IF IDARTNR-NY-WS NOT = ZERO                                      
061800            MOVE IDARTNR-NY-WS TO MOD-MOD-IDARTNR-NY                      
061900            INSPECT MOD-MOD-IDARTNR-NY REPLACING LEADING ZERO             
062000                      BY SPACE                                            
062100         END-IF                                                           
062200         IF MFS-IDTRANS = '111F'                                          
062300*****************  IDTRANS 111F ÄR FÖR BMP-UPPDAT FRÅN MID-FIL            
062400*****************  TILL 1116 OCH SKALL DÄRFÖR INTE SVARA SKÄRM            
062500           CONTINUE                                                       
062600         ELSE                                                             
062700           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
062800           PERFORM IMS-INSERT-MSG                                         
062900         END-IF                                                           
063000       END-IF                                                             
063100     END-IF                                                               
063200                                                                          
063300     MOVE ZERO TO RETURN-CODE                                             
063400     GOBACK                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 A-INIT SECTION.                                                          
063800     SKIP2                                                                
063900     IF MSG-DUBBLA-TRANSKODER                                             
064000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO INAREA                       
064100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
064200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
064300       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
064400     ELSE                                                                 
064500       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO INAREA                         
064600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
064700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
064800       MOVE ' ' TO MFS-KDTRTYP                                            
064900     END-IF                                                               
065000                                                                          
065100     IF MID-IDARTNR-IN = ALL '+' OR SPACE                                 
065200       INSPECT MID-IDARTNR-UT REPLACING LEADING                           
065300                    SPACE BY ZERO                                         
065400       MOVE MID-IDARTNR-UT TO WS-IDARTNR                                  
065500     ELSE                                                                 
065600       MOVE MID-IDARTNR-IN TO WS-IDARTNR                                  
065700       MOVE SPACE TO MFS-KDTRTYP                                          
065800     END-IF                                                               
065900                                                                          
066000     IF MID-IDARTNR-NY = ALL '+' OR SPACE                                 
066100****    MOVE ZERO TO WS-NY-IDARTNR                                        
066200        MOVE WS-IDARTNR     TO WS-NY-IDARTNR                              
066300     ELSE                                                                 
066400       INSPECT MID-IDARTNR-NY REPLACING LEADING SPACE BY ZERO             
066500       IF MID-IDARTNR-NY NUMERIC                                          
066600         MOVE MID-IDARTNR-NY TO WS-NY-IDARTNR                             
066700       ELSE                                                               
066800         MOVE ZERO TO WS-NY-IDARTNR                                       
066900       END-IF                                                             
067000     END-IF                                                               
067100                                                                          
067200     ACCEPT DAGENS-DATUM FROM DATE                                        
067300                                                                          
067400     MOVE DAGENS-DATUM          TO DAT-I-TIDATUM                          
067500     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
067600     CALL WDATKONV USING DAT-KDDATFORM                                    
067700                         DAT-I-TIDATUM                                    
067800                         DAT-O-TIDATUM                                    
067900                         DAT-KDSVAR                                       
068000                                                                          
068100     IF DAT-KDSVAR-OK                                                     
068200        MOVE DAT-TIAA-VECKA     TO SPAR-DAGENS-AA                         
068300        MOVE DAT-TIVV           TO SPAR-DAGENS-VV                         
068400     END-IF                                                               
068500                                                                          
068600     MOVE LOW-VALUE TO MSG-AREA                                           
068700     MOVE MOD-W1O11601 TO MOD-MOD-W1O11601                                
068800     IF MFS-IDTRANS = '1116'                                              
068900       MOVE 'W1O11601' TO MFS-IDMOD                                       
069000       MOVE '1116' TO MOD-MOD-IDTRANS                                     
069100     ELSE                                                                 
069200       MOVE 'W90410O1' TO MFS-IDMOD                                       
069300       MOVE '9410' TO MOD-MOD-IDTRANS                                     
069400     END-IF                                                               
069500                                                                          
069600     MOVE MFS-RENSA-FAELT TO MOD-MOD-IDARTNR-IN                           
069700                             MOD-MOD-IDARTNR-NY                           
069800                                                                          
069900     MOVE SPACE           TO WS-KDAGE                                     
070000     .                                                                    
070100     EJECT                                                                
070200 B-LAS-ARTIKEL SECTION.                                                   
070300     SKIP2                                                                
070400     MOVE IDARTNR-WS TO W-IDARTNR                                         
070500     PERFORM IMS-GET-ARTC01                                               
070600     IF SEGMENT-FINNS                                                     
070700        MOVE ART-FLERS     TO WS-FLERS                                    
070800        MOVE ART-KDERS-UTG TO WS-KDERS-UTG                                
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200 C-VAECKNING-AV-GAMMAL-ARTIKEL SECTION.                                   
071300     SKIP2                                                                
071400     PERFORM S01-KOLLA-SKAERMEN                                           
071500     PERFORM S02-UPPDATERA-ARTREG                                         
071600*           WS-KDPRODSL SATT                                              
071700     PERFORM S08-VAECKNING-RASA                                           
071800     PERFORM S03-UPPDATERA-KDPFIL                                         
071900     PERFORM CA-TRANS-TILL-VR                                             
072000     .                                                                    
072100     EJECT                                                                
072200 CA-TRANS-TILL-VR SECTION.                                                
072300     SKIP2                                                                
072400***** ÄT JULI 93                                                          
072500                                                                          
072600     MOVE LOW-VALUE      TO IO-AREA                                       
072700     MOVE IDARTNR-WS     TO XXID-IDARTNR                                  
072800     MOVE WS-KDERS-UTG   TO XXID-KDERS-OLD                                
072900     MOVE ZERO           TO XXID-KDERS-NEW                                
073000                                                                          
073100     PERFORM IMS-ISRT-XXID                                                
073200     .                                                                    
073300     EJECT                                                                
073400 D-NYREGISTRERING SECTION.                                                
073500     SKIP2                                                                
073600     PERFORM S01-KOLLA-SKAERMEN                                           
073700     PERFORM S02-UPPDATERA-ARTREG                                         
073800*           WS-KDPRODSL SATT                                              
073900     PERFORM S07-UPPDATERA-RASA                                           
074000                                                                          
074100     PERFORM S03-UPPDATERA-KDPFIL                                         
074200     PERFORM S05-IDSKYLT-BEART-FLRSBEART                                  
074300     PERFORM S06-UPPDATERA-BENREG                                         
074400     .                                                                    
074500     EJECT                                                                
074600 E-KOPIERA-ARTIKEL SECTION.                                               
074700     SKIP2                                                                
074800     PERFORM EA-KOLLA-SKAERMEN-KOP                                        
074900     PERFORM EB-KOPIERA-ARTIKEL                                           
075000*           WS-KDPRODSL SATT                                              
075100     PERFORM S05-IDSKYLT-BEART-FLRSBEART                                  
075200     PERFORM S06-UPPDATERA-BENREG                                         
075300     PERFORM S04-UPPDAT-CROSSIND-LISTPOST                                 
075400     PERFORM S07-UPPDATERA-RASA                                           
075500                                                                          
075600     MOVE IDARTNR-NY-WS TO IDARTNR-WS                                     
075700     PERFORM S03-UPPDATERA-KDPFIL                                         
075800     .                                                                    
075900     EJECT                                                                
076000 EA-KOLLA-SKAERMEN-KOP SECTION.                                           
076100     SKIP2                                                                
076200     MOVE SPACE TO INMATAD-IDKAT                                          
076300                                                                          
076400     IF MID-IDLEVNR = ALL '+' AND MID-BELEV = ALL '+'                     
076500        MOVE SPACE TO WS-IDLEVNR                                          
076600                      WS-BELEV                                            
076700     ELSE                                                                 
076800        MOVE MID-IDLEVNR TO WS-IDLEVNR                                    
076900        MOVE MID-BELEV TO WS-BELEV                                        
077000     END-IF                                                               
077100     IF MID-IDLEVNR = '1002 '                                             
077200        IF MID-BELEV = ALL '+' OR SPACE                                   
077300           MOVE SPACE TO WS-IDLEVNR                                       
077400                         WS-BELEV                                         
077500        END-IF                                                            
077600     END-IF                                                               
077700                                                                          
077800     IF MID-FLGAMART = ALL '+'                                            
077900       MOVE NEJ TO WS-FLGAMART                                            
078000     ELSE                                                                 
078100       MOVE MID-FLGAMART TO WS-FLGAMART                                   
078200     END-IF                                                               
078300                                                                          
078400     IF MID-IDRITN = ALL '+'                                              
078500        CONTINUE                                                          
078600     ELSE                                                                 
078700       MOVE MID-IDRITN TO WS-IDRITN                                       
078800     END-IF                                                               
078900                                                                          
079000     IF MID-IDPROJ = ALL '+'                                              
079100        CONTINUE                                                          
079200     ELSE                                                                 
079300       MOVE MID-IDPROJ TO WS-IDPROJ                                       
079400     END-IF                                                               
079500                                                                          
079600     MOVE MID-IDPROENH-1 TO WS-IDPROENH(1)                                
079700     MOVE MID-IDPROENH-2 TO WS-IDPROENH(2)                                
079800     MOVE MID-IDPROENH-3 TO WS-IDPROENH(3)                                
079900                                                                          
080000     IF MID-IDBERED = ALL '+'                                             
080100        CONTINUE                                                          
080200     ELSE                                                                 
080300       MOVE MID-IDBERED TO WS-IDBERED                                     
080400     END-IF                                                               
080500                                                                          
080600     IF MID-KDSORT = ALL '+'                                              
080700        CONTINUE                                                          
080800     ELSE                                                                 
080900         MOVE MID-KDSORT TO WS-KDSORT                                     
081000     END-IF                                                               
081100                                                                          
081200     IF MID-IDAO = ALL '+'                                                
081300        CONTINUE                                                          
081400     ELSE                                                                 
081500       MOVE MID-IDAO TO WS-IDAO                                           
081600     END-IF                                                               
081700                                                                          
081800     MOVE MID-TISOP   TO XX-TISOP                                         
081900     MOVE '+' TO XX-DAG-SOP                                               
082000     IF XX-TISOP   = ALL '+'                                              
082100        CONTINUE                                                          
082200     ELSE                                                                 
082300        MOVE MID-TISOP   TO XX-TISOP                                      
082400                                                                          
082500        IF  XX-AAR-SOP = '99'                                             
082600        AND XX-VECKA = '99'                                               
082700           MOVE '9' TO XX-DAG-SOP                                         
082800        ELSE                                                              
082900           MOVE '1' TO XX-DAG-SOP                                         
083000        END-IF                                                            
083100                                                                          
083200        MOVE XX-TISOP   TO WS-TISOP                                       
083300        PERFORM S25-TIFINLV-FRAN-SOP                                      
083400     END-IF                                                               
083500                                                                          
083600     IF MID-IDFKNGRP = ALL '+'                                            
083700        CONTINUE                                                          
083800     ELSE                                                                 
083900        MOVE MID-IDFKNGRP TO WS-IDFKNGRP                                  
084000     END-IF                                                               
084100                                                                          
084200     IF MID-TEARTNOT-2 = ALL '+'                                          
084300       MOVE SPACE TO WS-TEARTNOT-2                                        
084400     ELSE                                                                 
084500       MOVE MID-TEARTNOT-2 TO WS-TEARTNOT-2                               
084600     END-IF                                                               
084700                                                                          
084800     IF MID-TEARTNOT-7 = ALL '+'                                          
084900       MOVE SPACE TO WS-TEARTNOT-7                                        
085000     ELSE                                                                 
085100       MOVE MID-TEARTNOT-7 TO WS-TEARTNOT-7                               
085200     END-IF                                                               
085300                                                                          
085400     IF MID-KDPRODSL = ALL '+'                                            
085500        CONTINUE                                                          
085600     ELSE                                                                 
085700       MOVE MID-KDPRODSL TO WS-KDPRODSL                                   
085800     END-IF                                                               
085900                                                                          
086000     IF MID-TEARTNOT-4 = ALL '+'                                          
086100       MOVE SPACE TO WS-TEARTNOT-4                                        
086200     ELSE                                                                 
086300       MOVE MID-TEARTNOT-4 TO WS-TEARTNOT-4                               
086400     END-IF                                                               
086500                                                                          
086600     IF MID-IDKAT-1 = ALL '+'                                             
086700        CONTINUE                                                          
086800     ELSE                                                                 
086900        MOVE MID-IDKAT-1 TO WS-IDKAT(1)                                   
087000     END-IF                                                               
087100                                                                          
087200     IF MID-IDKAT-2 = ALL '+'                                             
087300        CONTINUE                                                          
087400     ELSE                                                                 
087500        MOVE MID-IDKAT-2 TO WS-IDKAT(2)                                   
087600     END-IF                                                               
087700                                                                          
087800     IF MID-IDKAT-3 = ALL '+'                                             
087900        CONTINUE                                                          
088000     ELSE                                                                 
088100        MOVE MID-IDKAT-3 TO WS-IDKAT(3)                                   
088200     END-IF                                                               
088300                                                                          
088400     IF MID-IDPROJUP = ALL '+'                                            
088500        CONTINUE                                                          
088600     ELSE                                                                 
088700       MOVE MID-IDPROJUP TO WS-IDPROJUP                                   
088800     END-IF                                                               
088900                                                                          
089000     IF MID-KDUART = ALL '+'                                              
089100        CONTINUE                                                          
089200     ELSE                                                                 
089300        MOVE MID-KDUART TO WS-KDUART                                      
089400     END-IF                                                               
089500                                                                          
089600     IF MID-KDYTBEH = ALL '+'                                             
089700        CONTINUE                                                          
089800     ELSE                                                                 
089900        MOVE MID-KDYTBEH TO WS-KDYTBEH                                    
090000     END-IF                                                               
090100                                                                          
090200     IF MID-KDFARLIG = ALL '+'                                            
090300        CONTINUE                                                          
090400     ELSE                                                                 
090500        MOVE MID-KDFARLIG TO WS-KDFARLIG                                  
090600     END-IF                                                               
090700                                                                          
090800     IF MID-KDBPSR = ALL '+'                                              
090900        CONTINUE                                                          
091000     ELSE                                                                 
091100        MOVE MID-KDBPSR TO WS-KDBPSR                                      
091200     END-IF                                                               
091300                                                                          
091400     IF MID-FLLSRDEL = ALL '+'                                            
091500        CONTINUE                                                          
091600     ELSE                                                                 
091700       MOVE MID-FLLSRDEL TO WS-FLLSRDEL                                   
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100 EB-KOPIERA-ARTIKEL SECTION.                                              
092200     SKIP2                                                                
092300     MOVE 'IDAG  '   TO DAT-KDDATFORM                                     
092400     CALL WDATKONV USING DAT-KDDATFORM                                    
092500                         DAT-I-TIDATUM                                    
092600                         DAT-O-TIDATUM                                    
092700                         DAT-KDSVAR                                       
092800                                                                          
092900     IF DAT-KDSVAR-OK                                                     
093000        MOVE DAT-TIAAVVD      TO AAVVD                                    
093100        MOVE AAVV             TO W009VADD-DATUM                           
093200        MOVE +6               TO W009VADD-ANTAL                           
093300        CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                 
093400     ELSE                                                                 
093500        MOVE ZERO             TO W009VADD-DATUM                           
093600     END-IF                                                               
093700                                                                          
093800*** WDK601                                                                
093900                                                                          
094000     MOVE IDARTNR-WS TO W-IDARTNR                                         
094100     PERFORM IMS-GHU-ARTC01                                               
094200                                                                          
094300     IF MID-IDFKNGRP = ALL '+'                                            
094400        MOVE ART-IDFKNGRP      TO WS-IDFKNGRP                             
094500     END-IF                                                               
094600                                                                          
094700     IF MID-KDPRODSL = ALL '+'                                            
094800        MOVE ART-KDPRODSL       TO WS-KDPRODSL                            
094900     END-IF                                                               
095000                                                                          
095100     IF MID-KDSORT = ALL '+'                                              
095200        MOVE ART-KDSORT         TO WS-KDSORT                              
095300     END-IF                                                               
095400                                                                          
095500     MOVE IDARTNR-NY-WS         TO ART-IDARTNR                            
095600                                                                          
095700     MOVE IDARTNR-NY-WS TO RESK-IDART                                     
095800     CALL W009KSIF USING RESK-IDART RESK-9-POS RESK-W009KSIFR             
095900     MOVE RESK-W009KSIFR         TO ART-REKSIFFR                          
096000                                                                          
096100     MOVE NEJ                   TO ART-FLERS                              
096200     MOVE ZERO                  TO ART-KDERS-UTG                          
096300     MOVE ZERO                  TO ART-TIERSDAT                           
096400                                   ART-TIURPROD                           
096410                                   ART-KDANSKSEG                          
096420     MOVE SPACE                 TO ART-IDCDS                              
096430                                   ART-KDARTSYS                           
096500     IF MID-IDLEVNR = '1002 '                                             
096600        MOVE MID-IDLEVNR        TO ART-IDLEVNR                            
096700     ELSE                                                                 
096800        MOVE SPACE              TO ART-IDLEVNR                            
096900     END-IF                                                               
097000     MOVE DAGENS-DATUM          TO ART-TIREGDAT                           
097100     MOVE NEJ                   TO ART-FLIART                             
097200                                                                          
097300     MOVE WS-IDFKNGRP           TO ART-IDFKNGRP                           
097400     MOVE WS-KDPRODSL           TO ART-KDPRODSL                           
097500     MOVE WS-KDSORT             TO ART-KDSORT                             
097600     PERFORM S98-HAEMTA-IDFTG                                             
097700     MOVE KPS-IDFTG             TO ART-IDFTG                              
097800                                                                          
097900     IF XX-TISOP   = ALL '+'                                              
098000        CONTINUE                                                          
098100     ELSE                                                                 
098200       MOVE WS-TISOP            TO ART-TISOP                              
098300       MOVE SPAR-TIFINLV-AAVVD  TO ART-TIFINLV                            
098400     END-IF                                                               
098500                                                                          
098600     IF MID-IDAO = ALL '+'                                                
098700       MOVE SPACE                 TO ART-IDAO(2)                          
098800       MOVE SPACE                 TO ART-IDAO(3)                          
098900       MOVE SPACE                 TO ART-IDAO(4)                          
099000       MOVE SPACE                 TO ART-IDAO(5)                          
099100     ELSE                                                                 
099200       MOVE WS-IDAO               TO ART-IDAO(1)                          
099300       MOVE SPACE                 TO ART-IDAO(2)                          
099400       MOVE SPACE                 TO ART-IDAO(3)                          
099500       MOVE SPACE                 TO ART-IDAO(4)                          
099600       MOVE SPACE                 TO ART-IDAO(5)                          
099700     END-IF                                                               
099800                                                                          
099900     MOVE 'N'                     TO ART-FLBRAND                          
100000     MOVE 'N'                     TO ART-FLBSNES                          
100100     MOVE  0                      TO ART-KDSOP                            
100200     MOVE 15                      TO ART-KVEOP                            
100201                                                                          
100202     MOVE ART-TIFINLV             TO WS-TIFINLV                           
100203     MOVE WS-TIFINLV              TO MOD-MOD-TIFINLV-UT                   
100204     MOVE ART-TISOP               TO WS-TISOP                             
100205     MOVE WS-TISOP                TO MOD-MOD-TISOP-UT                     
100300     MOVE ART-IDAO(1)             TO MOD-MOD-IDAO-UT                      
100400     MOVE ART-IDFKNGRP            TO WS-IDFKNGRP                          
100500     MOVE WS-IDFKNGRP             TO MOD-MOD-IDFKNGRP-UT                  
100600     MOVE ART-KDPRODSL            TO WS-KDPRODSL                          
100700     MOVE WS-KDPRODSL             TO MOD-MOD-KDPRODSL-UT                  
100800     MOVE ART-KDSORT              TO MOD-MOD-KDSORT-UT                    
100801                                                                          
100802*** CALL W221SEGM TO SET KDANSKSEG                                        
100840     MOVE 001                 TO SEGM-KDCALL                              
100850     MOVE ART-IDARTNR         TO SEGM-IDARTNR                             
100860     MOVE ZERO                TO SEGM-KDANSKSEG-IN                        
100870     MOVE SPACE               TO SEGM-IDDC                                
100880     MOVE SPACE               TO SEGM-IDREFTAB-IN                         
100890     MOVE WS-KDPRODSL         TO SEGM-KDPRODSL                            
100891     MOVE ART-FLBSNES         TO SEGM-FLBSNES                             
100892     MOVE ART-KVEOP           TO SEGM-KVEOP                               
100893     MOVE ZERO                TO SEGM-KDVSOP                              
100894     MOVE ART-TISOP           TO SEGM-TISOP                               
100895     MOVE ZERO                TO SEGM-TIURPROD                            
100896     MOVE WS-KDFARLIG         TO SEGM-KDFARLIG                            
100898     CALL W221SEGM USING SEGM-W221SEGM                                    
100899                         SEGM-WDB6-PCB                                    
100900                         SEGM-WDL7-PCB                                    
100901                         SEGM-WDL8-PCB                                    
100902                         SEGM-WDD5-PCB                                    
100903     IF  SEGM-KDSVAR-OK                                                   
100904         IF SEGM-FLANSKSEG-CHANGED = JA                                   
100905            MOVE SEGM-KDANSKSEG    TO ART-KDANSKSEG                       
100906         ELSE                                                             
100907            MOVE SEGM-KDANSKSEG-IN TO ART-KDANSKSEG                       
100908         END-IF                                                           
100910     ELSE                                                                 
100911         DISPLAY 'W221SEGM-ERROR1:' SEGM-TEXT                             
100912         CALL FELLOG                                                      
100913     END-IF                                                               
100920                                                                          
101000     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
101100     PERFORM IMS-ISRT-ARTC01                                              
101200                                                                          
101300*** WDK611                                                                
101400                                                                          
101500     MOVE MFS-RENSA-FAELT      TO MOD-MOD-IDPROENH-1-UT                   
101600                                  MOD-MOD-IDPROENH-2-UT                   
101700                                  MOD-MOD-IDPROENH-3-UT                   
101800     PERFORM S100-LAS-KDAGE                                               
101900     PERFORM S10-KOLLA-KDPSLLOC                                           
102000                                                                          
102100     MOVE IDARTNR-WS TO W-IDARTNR                                         
102200     PERFORM IMS-GHU-ARTC01                                               
102300     PERFORM IMS-GET-ARTC11-KOP                                           
102400                                                                          
102500     IF MID-IDLEVNR = '1002 '                                             
102600        MOVE MID-IDLEVNR        TO CLAG-IDLEVNR-SHIP                      
102700     ELSE                                                                 
102800        MOVE SPACE              TO CLAG-IDLEVNR-SHIP                      
102900     END-IF                                                               
103000     MOVE '1'                   TO CLAG-KDSEGKEY                          
103100     MOVE ZERO                  TO CLAG-ADLAGOMR                          
103200     MOVE ZERO                  TO CLAG-ADGANG                            
103300     MOVE ZERO                  TO CLAG-ADPLATS                           
103400     MOVE ZERO                  TO CLAG-ADLAGOMR-SVS                      
103500     MOVE ZERO                  TO CLAG-ADGANG-SVS                        
103600     MOVE ZERO                  TO CLAG-ADPLATS-SVS                       
103700     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(1)                    
103800     MOVE ZERO                  TO CLAG-ADGANG-CD(1)                      
103900     MOVE ZERO                  TO CLAG-ADPLATS-CD(1)                     
104000     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(2)                    
104100     MOVE ZERO                  TO CLAG-ADGANG-CD(2)                      
104200     MOVE ZERO                  TO CLAG-ADPLATS-CD(2)                     
104300     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(3)                    
104400     MOVE ZERO                  TO CLAG-ADGANG-CD(3)                      
104500     MOVE ZERO                  TO CLAG-ADPLATS-CD(3)                     
104600     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(4)                    
104700     MOVE ZERO                  TO CLAG-ADGANG-CD(4)                      
104800     MOVE ZERO                  TO CLAG-ADPLATS-CD(4)                     
104900     MOVE SPACE                 TO CLAG-ADINPORT                          
105000     MOVE ZERO                  TO CLAG-DAXPOINT                          
105100     MOVE ZERO                  TO CLAG-KDEXCHA                           
105200     MOVE ZERO                  TO CLAG-KVPOINT                           
105300     MOVE ZERO                  TO CLAG-PRREF                             
105400     MOVE 'N'                   TO CLAG-FLAVRART                          
105500     MOVE 'N'                   TO CLAG-FLFSP                             
105600     MOVE 'N'                   TO CLAG-FLJIT                             
105700     MOVE 'N'                   TO CLAG-FLMANAT                           
105800     MOVE 'N'                   TO CLAG-FLMANBK                           
105900     MOVE 'N'                   TO CLAG-FLMANKP                           
106000     MOVE 'N'                   TO CLAG-FLMANLT                           
106100     MOVE 'N'                   TO CLAG-FLMANQ                            
106200     MOVE 'N'                   TO CLAG-FLGEMART                          
106300     MOVE 'N'                   TO CLAG-FLMANOSK                          
106400     MOVE 'N'                   TO CLAG-FLLARM-BUF                        
106500     MOVE 'N'                   TO CLAG-FLMPB                             
106600     MOVE 'N'                   TO CLAG-FLOREGPB                          
106700     MOVE 'N'                   TO CLAG-FLSKROT-BEORD                     
106800     MOVE 'N'                   TO CLAG-FLMANPB                           
106900     MOVE 'N'                   TO CLAG-FLSPKOST                          
107000     MOVE 'N'                   TO CLAG-FLLTKSP                           
107100     MOVE 'N'                   TO CLAG-FLMANGK                           
107200     MOVE 'N'                   TO CLAG-FLTOPP                            
107300     MOVE 'N'                   TO CLAG-FLMANOPP                          
107400     MOVE 'N'                   TO CLAG-KDOPPLAN                          
107500     MOVE 'N'                   TO CLAG-FLRADREF                          
107600     MOVE 'N'                   TO CLAG-FLTPO1                            
107700     MOVE 'N'                   TO CLAG-FLRELSP                           
107800     MOVE 'N'                   TO CLAG-FLMARKSP                          
107900     MOVE 'N'                   TO CLAG-FLEJBUFF                          
108000     MOVE 'N'                   TO CLAG-FLCDART                           
108100     MOVE 'N'                   TO CLAG-FLNYBER                           
108200     MOVE 'N'                   TO CLAG-FLSKROT-AUTO                      
108300     MOVE 'N'                   TO CLAG-FLSKROT-BEV                       
108400     MOVE 'N'                   TO CLAG-FLSKROT-SL                        
108500     MOVE 'N'                   TO CLAG-FLSKROT-WLC                       
108510     MOVE 'N'                   TO CLAG-FLAUTREL                          
108600     MOVE ZERO                  TO CLAG-KDUVKNTO                          
108700     MOVE WS-KDPRODSL           TO TEST-KDPRODSL                          
108800     IF KDPRODSL-VOLVO-BIMA                                               
108900        MOVE 'J'                TO CLAG-FLNYBER                           
109000     END-IF                                                               
109100                                                                          
109200     IF MID-FLLSRDEL = ALL '+'                                            
109300        MOVE CLAG-FLLSRDEL      TO WS-FLLSRDEL                            
109400     ELSE                                                                 
109500        MOVE WS-FLLSRDEL        TO CLAG-FLLSRDEL                          
109600     END-IF                                                               
109700                                                                          
109800     PERFORM S09-KOLLA-FLREFILL                                           
109900                                                                          
110000     IF MID-IDBERED = ALL '+'                                             
110100       MOVE CLAG-IDBERED        TO WS-IDBERED                             
110200     ELSE                                                                 
110300       MOVE WS-IDBERED          TO CLAG-IDBERED                           
110400     END-IF                                                               
110500     IF MID-IDPROJ = ALL '+'                                              
110600        MOVE CLAG-IDPROJ        TO WS-IDPROJ                              
110700     ELSE                                                                 
110800        MOVE WS-IDPROJ          TO CLAG-IDPROJ                            
110900     END-IF                                                               
111000     IF MID-IDPROJUP = ALL '+'                                            
111100        CONTINUE                                                          
111200     ELSE                                                                 
111300        MOVE WS-IDPROJUP        TO CLAG-IDPROJUP                          
111400     END-IF                                                               
111500     MOVE ZERO                  TO CLAG-IDANSK                            
111600     MOVE SPACE                 TO CLAG-IDINK                             
111700                                                                          
111800     IF MID-IDRITN = ALL '+'                                              
111900        CONTINUE                                                          
112000     ELSE                                                                 
112100       IF WS-IDRITN = '='                                                 
112200*****    KOPIERA ARTIKELNR TILL RITNINGSNR                                
112300*****    ("KAPA AV" INLEDANDE NOLLOR)                                     
112400                                                                          
112500         MOVE WS-NY-IDARTNR TO WS-W009REDU-IN                             
112600         INSPECT WS-W009REDU-IN REPLACING LEADING ZERO BY SPACE           
112700         CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT                
112800         MOVE WS-W009REDU-UT      TO CLAG-IDRITN                          
112900       ELSE                                                               
113000          MOVE WS-IDRITN          TO CLAG-IDRITN                          
113100       END-IF                                                             
113200     END-IF                                                               
113300                                                                          
113400     IF MID-KDYTBEH = ALL '+'                                             
113500        MOVE CLAG-KDYTBEH         TO WS-KDYTBEH                           
113600     ELSE                                                                 
113700        MOVE WS-KDYTBEH           TO CLAG-KDYTBEH                         
113800     END-IF                                                               
113900     MOVE ZERO                    TO CLAG-BEFT                            
114000                                                                          
114100     IF MID-IDKAT-1 = ALL '+'                                             
114200        CONTINUE                                                          
114300     ELSE                                                                 
114400       MOVE WS-IDKAT(1)           TO CLAG-IDKAT(1)                        
114500       MOVE WS-IDKAT(2)           TO CLAG-IDKAT(2)                        
114600       MOVE WS-IDKAT(3)           TO CLAG-IDKAT(3)                        
114700     END-IF                                                               
114800                                                                          
114900     MOVE ZERO                    TO CLAG-IDPSN                           
115000                                     CLAG-IDPLANGR-AG                     
115100                                     CLAG-IDPLANGR-LEV                    
115200     MOVE SPACE                   TO CLAG-IDFS-SEN                        
115300                                     CLAG-IDLEVNR-SEN                     
115400                                     CLAG-IDDC-REF                        
115500     MOVE ZERO                    TO CLAG-IDLKTO                          
115600                                     CLAG-PRHEMTAG                        
115700     MOVE ZERO                    TO CLAG-IDSTATNR(1)                     
115800                                     CLAG-IDSTATNR(2)                     
115900                                     CLAG-IDSTATNR(3)                     
116000                                     CLAG-IDSTATNR(4)                     
116100                                     CLAG-IDSTATNR(5)                     
116200                                     CLAG-IDSTATNR(6)                     
116300                                     CLAG-IDARTNR-EMBQ0                   
116400                                     CLAG-IDARTNR-EMBQ1                   
116500                                     CLAG-IDARTNR-EMBQ2                   
116600                                     CLAG-IDARTNR-EMBQ3                   
116700                                     CLAG-IDARTNR-EMBQ4                   
116800                                     CLAG-TIUPPDAT-EMB                    
116900                                     CLAG-TIGILTIG-PCOO                   
117000                                     CLAG-TISTODAT-LARM                   
117100     MOVE SPACE                   TO CLAG-IDUSER-SPKVAL                   
117200                                     CLAG-IDUSER-EMB                      
117300                                     CLAG-KDPCOO                          
117400     MOVE +1 TO IX                                                        
117500     PERFORM UNTIL IX > 3                                                 
117600        IF WS-IDPROENH (IX) = ALL '+'                                     
117700           MOVE CLAG-IDPROENH(IX) TO WS-IDPROENH(IX)                      
117800        END-IF                                                            
117900        ADD +1 TO IX                                                      
118000     END-PERFORM                                                          
118100                                                                          
118200     MOVE +1 TO IX                                                        
118300     MOVE SPACE TO CLAG-IDPROENH(1)                                       
118400                   CLAG-IDPROENH(2)                                       
118500                   CLAG-IDPROENH(3)                                       
118600     PERFORM UNTIL IX > MAX-IX                                            
118700        IF WS-IDPROENH (IX) = ALL '+' OR SPACE                            
118800           CONTINUE                                                       
118900        ELSE                                                              
119000           MOVE WS-IDPROENH (IX) TO CLAG-IDPROENH(IX)                     
119100                                                                          
119200           IF IX = 1                                                      
119300              MOVE CLAG-IDPROENH(1) TO MOD-MOD-IDPROENH-1-UT              
119400              INSPECT MOD-MOD-IDPROENH-1-UT REPLACING                     
119500              LEADING ZERO BY SPACE                                       
119600           ELSE                                                           
119700              IF IX = 2                                                   
119800                 MOVE CLAG-IDPROENH(2) TO  MOD-MOD-IDPROENH-2-UT          
119900                 INSPECT MOD-MOD-IDPROENH-2-UT REPLACING                  
120000                 LEADING ZERO BY SPACE                                    
120100              ELSE                                                        
120200                 IF IX = 3                                                
120300                   MOVE CLAG-IDPROENH(3)TO MOD-MOD-IDPROENH-3-UT          
120400                   INSPECT MOD-MOD-IDPROENH-3-UT REPLACING                
120500                   LEADING ZERO BY SPACE                                  
120600                 END-IF                                                   
120700              END-IF                                                      
120800           END-IF                                                         
120900        END-IF                                                            
121000        ADD +1 TO IX                                                      
121100     END-PERFORM                                                          
121200                                                                          
121300     IF MID-KDUART = ALL '+'                                              
121400        MOVE CLAG-KDUART        TO WS-KDUART                              
121500     ELSE                                                                 
121600        MOVE WS-KDUART          TO CLAG-KDUART                            
121700     END-IF                                                               
121800     MOVE ZERO                  TO CLAG-KDAVT                             
121900     MOVE ZERO                  TO CLAG-KDHF                              
122000     MOVE +2                    TO CLAG-KDKSP                             
122100     MOVE ZERO                  TO CLAG-KDLPSP                            
122200                                   CLAG-KDVVKL                            
122300                                   CLAG-KVSPARR-KVAL                      
122400     MOVE WS-KDAGE              TO CLAG-KDAGE                             
122500     MOVE +1                    TO CLAG-KDKG                              
122600     MOVE ZERO                  TO CLAG-KDTIPPR                           
122700     MOVE ZERO                  TO CLAG-KDVTH                             
122800     MOVE ZERO                  TO CLAG-KDGK                              
122900     MOVE ZERO                  TO CLAG-KDLEVSP                           
123000     MOVE 'P'                   TO CLAG-KDLEVPLF                          
123100     MOVE WS-KDPSLLOC           TO CLAG-KDPSLLOC                          
123200     MOVE +1                    TO CLAG-KDLTK                             
123300     IF MID-KDFARLIG = ALL '+'                                            
123400        MOVE CLAG-KDFARLIG      TO WS-KDFARLIG                            
123500     ELSE                                                                 
123600        MOVE WS-KDFARLIG        TO CLAG-KDFARLIG                          
123700     END-IF                                                               
123800     IF MID-KDBPSR = ALL '+'                                              
123900        MOVE CLAG-KDBPSR        TO WS-KDBPSR                              
124000     ELSE                                                                 
124100        MOVE WS-KDBPSR          TO CLAG-KDBPSR                            
124200     END-IF                                                               
124300     MOVE ZERO                  TO CLAG-KDFORP                            
124400     MOVE SPACE                 TO CLAG-KDEFFMAN                          
124500     MOVE ZERO                  TO CLAG-KDEMBKOD-0                        
124600     MOVE ZERO                  TO CLAG-KDEMBKOD-1                        
124700     MOVE ZERO                  TO CLAG-KDEMBKOD-2                        
124800     MOVE ZERO                  TO CLAG-KDVSOP                            
124900     MOVE ZERO                  TO CLAG-KDTULLRE                          
125000     MOVE SPACE                 TO CLAG-KDARTURS                          
125100     MOVE ZERO                  TO CLAG-KDSRA                             
125200     MOVE ZERO                  TO CLAG-KDERS                             
125300     MOVE ZERO                  TO CLAG-KDSPEEMB                          
125400     MOVE ZERO                  TO CLAG-KDARTHNT                          
125500     MOVE SPACE                 TO CLAG-KDFREKKL                          
125600     MOVE SPACE                 TO CLAG-KDPRISKL                          
125700     MOVE SPACE                 TO CLAG-IDUSER-VUPD                       
125800     MOVE ZERO                  TO CLAG-KVDAGAR-FFH                       
125900     MOVE ZERO                  TO CLAG-KVAP                              
126000     MOVE ZERO                  TO CLAG-KVBK                              
126100     MOVE ZERO                  TO CLAG-KVDAGAR-INLEV                     
126200     MOVE ZERO                  TO CLAG-KVDAGAR-TT                        
126300     MOVE ZERO                  TO CLAG-KVKP                              
126400     MOVE ZERO                  TO CLAG-KVLAAN                            
126500     MOVE ZERO                  TO CLAG-KVOVERF                           
126600     MOVE ZERO                  TO CLAG-KVPALL                            
126700     MOVE ZERO                  TO CLAG-KVQ                               
126800     MOVE ZERO                  TO CLAG-KVQ-JUST                          
126900     MOVE ZERO                  TO CLAG-KVSLUTKP                          
127000     MOVE ZERO                  TO CLAG-KVVECKOR-AT                       
127100     MOVE ZERO                  TO CLAG-KVVECKOR-BT                       
127200     MOVE ZERO                  TO CLAG-KVVECKOR-FT                       
127300     MOVE ZERO                  TO CLAG-KVVECKOR-LT                       
127400     MOVE ZERO                  TO CLAG-KVVORKO                           
127500     MOVE ZERO                  TO CLAG-KVAVIS-SEN                        
127600     MOVE ZERO                  TO CLAG-KVMAD-SEP                         
127700     MOVE ZERO                  TO CLAG-KVMAD-TOT                         
127800     MOVE ZERO                  TO CLAG-KVMP                              
127900     MOVE ZERO                  TO CLAG-KVPB-SATS                         
128000     MOVE ZERO                  TO CLAG-KVPB-SEP                          
128100     MOVE ZERO                  TO CLAG-KVPB-TPO                          
128200     MOVE ZERO                  TO CLAG-KVPB-VESL                         
128300     MOVE ZERO                  TO CLAG-KVPB-HIST                         
128400     MOVE ZERO                  TO CLAG-KVUTJFEL                          
128500     MOVE ZERO                  TO CLAG-KVINVS                            
128600     MOVE ZERO                  TO CLAG-KVQPACK-0                         
128700     MOVE ZERO                  TO CLAG-KVQPACK-1                         
128800     MOVE ZERO                  TO CLAG-KVQPACK-2                         
128900     MOVE ZERO                  TO CLAG-KVQPACK-3                         
129000     MOVE ZERO                  TO CLAG-KVQPACK-4                         
129100     MOVE 8                     TO CLAG-KVFRYSTI                          
129200     MOVE ZERO                  TO CLAG-KVAKS-CDC                         
129300     MOVE ZERO                  TO CLAG-KVAKS-PAV                         
129400     MOVE ZERO                  TO CLAG-KVAKS-T                           
129500     MOVE ZERO                  TO CLAG-KVEFRS                            
129600     MOVE ZERO                  TO CLAG-KVLS                              
129700     MOVE ZERO                  TO CLAG-KVLS-SVS                          
129800     MOVE ZERO                  TO CLAG-KVLS-CD(1)                        
129900     MOVE ZERO                  TO CLAG-KVLS-CD(2)                        
130000     MOVE ZERO                  TO CLAG-KVLS-CD(3)                        
130100     MOVE ZERO                  TO CLAG-KVLS-CD(4)                        
130200     MOVE ZERO                  TO CLAG-KVOI-OVR                          
130300     MOVE ZERO                  TO CLAG-KVRESS-CD(1)                      
130400     MOVE ZERO                  TO CLAG-KVRESS-CD(2)                      
130500     MOVE ZERO                  TO CLAG-KVRESS-CD(3)                      
130600     MOVE ZERO                  TO CLAG-KVRESS-CD(4)                      
130700     MOVE ZERO                  TO CLAG-KVRESS                            
130800     MOVE ZERO                  TO CLAG-KVRETUR                           
130900     MOVE ZERO                  TO CLAG-KVROS                             
131000     MOVE ZERO                  TO CLAG-KVSLAGER                          
131100     MOVE ZERO                  TO CLAG-KVSPANT                           
131200     MOVE ZERO                  TO CLAG-KVUTRS                            
131300     MOVE ZERO                  TO CLAG-PRORDSK                           
131400     MOVE ZERO                  TO CLAG-PRARTSJK                          
131500     MOVE ZERO                  TO CLAG-PRARTSTD                          
131600     MOVE ZERO                  TO CLAG-PRARTBTO-EXP                      
131700     MOVE ZERO                  TO CLAG-PRDIRLON                          
131800     MOVE ZERO                  TO CLAG-PRDMTRL                           
131900     MOVE ZERO                  TO CLAG-PRINK                             
132000     MOVE ZERO                  TO CLAG-PROVRPAL                          
132100     MOVE ZERO                  TO CLAG-PRLFKST                           
132200     MOVE ZERO                  TO CLAG-REDIRLEV                          
132300     MOVE ZERO                  TO CLAG-RESLJUST                          
132400     MOVE ZERO                  TO CLAG-RETULF                            
132500     MOVE ZERO                  TO CLAG-RVPROFEL                          
132600     MOVE ZERO                  TO CLAG-RVPROURS                          
132700     MOVE ZERO                  TO CLAG-TIBESRPT                          
132800     MOVE W009VADD-DATUM        TO CLAG-TIBESRPT-PAAM                     
132900     MOVE ZERO                  TO CLAG-TILPSP                            
133000                                   CLAG-TIOMSPEC                          
133100                                   CLAG-TIQJUST                           
133200*****                              CLAG-TIURPROD                          
133300                                   CLAG-TISLUTKP                          
133400                                   CLAG-TIPBDAT                           
133500                                   CLAG-TISLJUST                          
133600                                   CLAG-TIINVDAT                          
133700                                   CLAG-TILTK                             
133800                                   CLAG-TIRODAT                           
133900                                   CLAG-TIDISPIN                          
134000                                   CLAG-TIAVIDAT-SEN                      
134100                                   CLAG-TIREFSTO                          
134200                                   CLAG-TISPARR-KVAL                      
134300                                   CLAG-TISTOREF                          
134400                                   CLAG-TIMAIL-KVAL                       
134500                                   CLAG-TISKROT                           
134600                                   CLAG-TIPBLOCK                          
134700                                   CLAG-VKART                             
134800                                   CLAG-VLARTNTO                          
134900                                   CLAG-VKART-NTO                         
135000                                   CLAG-KVEOQ                             
135100                                   CLAG-KVULOAD                           
135200                                   CLAG-KVSLAGER-OPT                      
135300                                   CLAG-KVVECKOR-LVAR                     
135400                                   CLAG-KVPB-TREND                        
135500                                   CLAG-KVVECKOR-TREND                    
135600                                   CLAG-TIDATUM-TREND                     
135700                                   CLAG-TISKROT-AUTO                      
135800                                   CLAG-TISKPREL                          
135900                                   CLAG-KVBEART                           
136000                                   CLAG-KVAVROP-TOT                       
136100                                   CLAG-KVREFOVL-TOT                      
136200                                   CLAG-KVRETUR-TOT                       
136300                                   CLAG-KVTILLG-TOT                       
136400                                   CLAG-TIUPPDAT-VUPD                     
136500****                                                                      
136600     MOVE ZERO                  TO CLAG-DAPBPLAN                          
136700                                   CLAG-DASEASON                          
136800                                   CLAG-KVPB-PLAN                         
136900                                   CLAG-KVREFBER-PLOCK                    
137000                                   CLAG-KVREFPKT-PLOCK                    
137100                                   CLAG-KVOI-PLOCK                        
137200                                   CLAG-KVMAXPL                           
137300                                   CLAG-KVPB-PLAN-JUST1                   
137400                                   CLAG-TIPBPLAN-JUST1-FOM                
137500                                   CLAG-TIPBPLAN-JUST1-TOM                
137600                                   CLAG-KVPB-PLAN-JUST2                   
137700                                   CLAG-TIPBPLAN-JUST2-FOM                
137800                                   CLAG-TIPBPLAN-JUST2-TOM                
137900     MOVE 1.00                  TO CLAG-RESEASON-PLAN(1)                  
138000                                   CLAG-RESEASON-PLAN(2)                  
138100                                   CLAG-RESEASON-PLAN(3)                  
138200                                   CLAG-RESEASON-PLAN(4)                  
138300                                   CLAG-RESEASON-PLAN(5)                  
138400                                   CLAG-RESEASON-PLAN(6)                  
138500                                   CLAG-RESEASON-PLAN(7)                  
138600                                   CLAG-RESEASON-PLAN(8)                  
138700                                   CLAG-RESEASON-PLAN(9)                  
138800                                   CLAG-RESEASON-PLAN(10)                 
138900                                   CLAG-RESEASON-PLAN(11)                 
139000                                   CLAG-RESEASON-PLAN(12)                 
139100                                                                          
139200     MOVE ZERO                  TO CLAG-TILEVDAG (1)                      
139300                                   CLAG-TILEVDAG (2)                      
139400                                   CLAG-TILEVDAG (3)                      
139500                                   CLAG-TILEVDAG (4)                      
139600                                   CLAG-TILEVDAG (5)                      
139700     MOVE SPACE                 TO CLAG-ADINLOMR-BOA                      
139800****                                                                      
139900                                                                          
140000     MOVE CLAG-IDBERED          TO WS-IDBERED                             
140100     MOVE WS-IDBERED            TO MOD-MOD-IDBERED-UT                     
140200     MOVE CLAG-IDPROJ           TO MOD-MOD-IDPROJ-UT                      
140300     MOVE CLAG-IDPROJUP         TO MOD-MOD-IDPROJUP-UT                    
140400     MOVE CLAG-IDRITN           TO MOD-MOD-IDRITN-UT                      
140500     MOVE CLAG-KDYTBEH          TO WS-KDYTBEH                             
140600     MOVE WS-KDYTBEH            TO MOD-MOD-KDYTBEH-UT                     
140700     MOVE CLAG-IDKAT(1)         TO MOD-MOD-IDKAT-1-UT                     
140800     MOVE CLAG-IDKAT(2)         TO MOD-MOD-IDKAT-2-UT                     
140900     MOVE CLAG-IDKAT(3)         TO MOD-MOD-IDKAT-3-UT                     
141000     MOVE CLAG-FLLSRDEL         TO MOD-MOD-FLLSRDEL-UT                    
141100     MOVE CLAG-KDUART           TO MOD-MOD-KDUART-UT                      
141200     MOVE CLAG-KDFARLIG         TO WS-KDFARLIG                            
141300     MOVE WS-KDFARLIG           TO MOD-MOD-KDFARLIG-UT                    
141400     MOVE CLAG-KDBPSR           TO WS-KDBPSR                              
141500     MOVE WS-KDBPSR             TO MOD-MOD-KDBPSR-UT                      
141600                                                                          
141700     MOVE IDARTNR-NY-WS TO W-IDARTNR                                      
141800     PERFORM IMS-ISRT-ARTC11                                              
141900                                                                          
142000*** WDK625                                                                
142100                                                                          
142200     IF WS-TEARTNOT-2 NOT = SPACE                                         
142300        MOVE WS-TEARTNOT-2      TO NOT-TEARTNOT                           
142400        MOVE +3                 TO NOT-KDNOTTYP                           
142500        PERFORM IMS-ISRT-ARTC25                                           
142600     END-IF                                                               
142700                                                                          
142800     IF WS-TEARTNOT-4 NOT = SPACE                                         
142900        MOVE WS-TEARTNOT-4      TO NOT-TEARTNOT                           
143000        MOVE +1                 TO NOT-KDNOTTYP                           
143100        PERFORM IMS-ISRT-ARTC25                                           
143200     END-IF                                                               
143300                                                                          
143400     IF WS-TEARTNOT-7 NOT = SPACE                                         
143500        MOVE WS-TEARTNOT-7      TO NOT-TEARTNOT                           
143600        MOVE +7                 TO NOT-KDNOTTYP                           
143700        PERFORM IMS-ISRT-ARTC25                                           
143800     END-IF                                                               
143900                                                                          
144000     MOVE WS-TEARTNOT-2         TO MOD-MOD-TEARTNOT-2                     
144100     MOVE WS-TEARTNOT-4         TO MOD-MOD-TEARTNOT-4                     
144200     MOVE WS-TEARTNOT-7         TO MOD-MOD-TEARTNOT-7                     
144300     .                                                                    
144400     EJECT                                                                
144500 F-KOEHANTERING SECTION.                                                  
144600     SKIP2                                                                
144700     MOVE IDARTNR-WS TO W-IDARTNR                                         
144800     PERFORM IMS-GET-ARTG01                                               
144900     IF SEGMENT-FINNS                                                     
145000        MOVE JA                 TO SW-NYPON-ART                           
145100        MOVE ARTG01-ART-FLBYTES TO WS-FLBYTES                             
145200        IF ARTG01-ART-TINEDBRY > ZERO                                     
145300           CONTINUE                                                       
145400        ELSE                                                              
145500           IF ARTG01-ART-FLBERQ = NEJ                                     
145600              CONTINUE                                                    
145700           ELSE                                                           
145800              MOVE NEJ TO ARTG01-ART-FLBERQ                               
145900           END-IF                                                         
146000        END-IF                                                            
146100                                                                          
146200        IF WS-KDBPSR = 8 AND WS-FLLSRDEL = NEJ                            
146300           CONTINUE                                                       
146400        ELSE                                                              
146500            MOVE +1 TO ARTG01-ART-KDANSKQ                                 
146600            MOVE WS-KDPRODSL   TO TEST-KDPRODSL                           
146700            IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                         
146800              IF WS-FLLSRDEL = JA                                         
146900                IF WS-KDUART = SPACE                                      
147000                  IF WS-KDBPSR = 1 OR 2 OR 3                              
147100                     MOVE +1 TO ARTG01-ART-DABASL                         
147200                  END-IF                                                  
147300                END-IF                                                    
147400              END-IF                                                      
147500            END-IF                                                        
147600                                                                          
147700            IF WS-KDSORT = 'SW'                                           
147800               MOVE 003 TO ARTG01-ART-IDANSK                              
147900            ELSE                                                          
148000                 PERFORM FA-HAEMTA-ANSK                                   
148100            END-IF                                                        
148200                                                                          
148300        END-IF                                                            
148400                                                                          
148500        IF WS-KDBPSR = 8 AND WS-FLLSRDEL = NEJ                            
148600           MOVE '-' TO ARTG01-ART-KDRESBED                                
148700        ELSE                                                              
148800           MOVE 'R' TO ARTG01-ART-KDRESBED                                
148900        END-IF                                                            
149000                                                                          
149100        PERFORM IMS-REPL-ARTG                                             
149200     END-IF                                                               
149300     .                                                                    
149400     EJECT                                                                
149500 FA-HAEMTA-ANSK SECTION.                                                  
149600     SKIP2                                                                
149700     MOVE NEJ TO WS-ANSK-FINNS                                            
149800     MOVE WS-KDPRODSL TO W-KDPRODSL-1137                                  
149900     PERFORM IMS-GET-XXAT01                                               
150000     IF SEGMENT-FINNS                                                     
150100        PERFORM IMS-GET-XXAT11                                            
150200        PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANSK-FINNS = JA                
150300           IF (WS-IDFKNGRP =  XXAT-1138-IDFKNGRP-FOM                      
150400               OR > XXAT-1138-IDFKNGRP-FOM) AND                           
150500               (WS-IDFKNGRP = XXAT-1138-IDFKNGRP-TOM                      
150600               OR < XXAT-1138-IDFKNGRP-TOM)                               
150700              MOVE XXAT-1138-IDANSK TO ARTG01-ART-IDANSK                  
150800              MOVE JA TO WS-ANSK-FINNS                                    
150900           ELSE                                                           
151000              PERFORM IMS-GET-XXAT11                                      
151100           END-IF                                                         
151200        END-PERFORM                                                       
151300     END-IF                                                               
151400     .                                                                    
151500     EJECT                                                                
151600 H-RENSA-INFAELT SECTION.                                                 
151700     SKIP2                                                                
151800     MOVE MFS-RENSA-FAELT TO MOD-MOD-IDBERED-IN                           
151900                             MOD-MOD-KDPRODSL-IN                          
152000                             MOD-MOD-KDSORT-IN                            
152100                             MOD-MOD-IDPROENH-1-IN                        
152200                             MOD-MOD-IDPROENH-2-IN                        
152300                             MOD-MOD-IDPROENH-3-IN                        
152400                             MOD-MOD-KDYTBEH-IN                           
152500                             MOD-MOD-IDPROJ-IN                            
152600                             MOD-MOD-KDFARLIG-IN                          
152700                             MOD-MOD-KDBPSR-IN                            
152800                             MOD-MOD-IDKAT-1-IN                           
152900                             MOD-MOD-IDKAT-2-IN                           
153000                             MOD-MOD-IDKAT-3-IN                           
153100                             MOD-MOD-KDUART-IN                            
153200                             MOD-MOD-FLPISK-IN                            
153300                             MOD-MOD-IDPROJK-IN                           
153400                             MOD-MOD-KVARTVAGN-IN                         
153500                             MOD-MOD-IDARTNR-MOTSV-IN                     
153600                             MOD-MOD-IDAO-IN                              
153700                             MOD-MOD-TISOP-IN                             
153800                             MOD-MOD-FLLSRDEL-IN                          
153900                             MOD-MOD-IDPROJUP-IN                          
154000                             MOD-MOD-BEART-IN                             
154100                             MOD-MOD-IDFKNGRP-IN                          
154200                             MOD-MOD-IDLEVNR-IN                           
154300                             MOD-MOD-BELEV-IN                             
154400                             MOD-MOD-IDRITN-IN                            
154500                             MOD-MOD-FLBYTES-IN                           
154600                             MOD-MOD-KVPROG-IN                            
154700     .                                                                    
154800     EJECT                                                                
154900 I-TRANS-TILL-HANDELSEREG SECTION.                                        
155000    SKIP2                                                                 
155100***WDR530                                                                 
155200     MOVE IDARTNR-WS            TO XXBW-2228-IDARTNR                      
155300     MOVE LOW-VALUE             TO XXBW-2228-LOW-VALUE                    
155400     MOVE SPACE                 TO XXBW-2228-FILLER                       
155500     PERFORM IMS-ISRT-XXBW                                                
155600     .                                                                    
155700     EJECT                                                                
155800 S01-KOLLA-SKAERMEN SECTION.                                              
155900     SKIP2                                                                
156000     MOVE SPACE TO INMATAD-IDKAT                                          
156100                                                                          
156200     MOVE IDARTNR-WS TO W-IDARTNR                                         
156300     PERFORM IMS-GET-ARTG01                                               
156400                                                                          
156500     IF SEGMENT-FINNS                                                     
156600        MOVE ARTG01-ART-IDRITN        TO WS-IDRITN                        
156700        MOVE ARTG01-ART-IDPROJ        TO WS-IDPROJ                        
156800        MOVE ARTG01-ART-IDPROENH      TO WS-IDPROENH(1)                   
156900        MOVE ARTG01-ART-IDBERED       TO WS-IDBERED                       
157000        MOVE ARTG01-ART-KDSORT        TO WS-KDSORT                        
157100        MOVE ARTG01-ART-IDAO          TO WS-IDAO                          
157200        MOVE ARTG01-ART-IDFKNGRP      TO WS-IDFKNGRP                      
157300        MOVE ARTG01-ART-TEARTNOT      TO WS-TEARTNOT-2                    
157400        MOVE ARTG01-ART-KDPRODSL      TO WS-KDPRODSL                      
157500                                                                          
157600        IF MID-IDPROJ = ALL '+'                                           
157700           MOVE ARTG01-ART-IDPROJ     TO W-IDPROJ                         
157800        ELSE                                                              
157900          MOVE MID-IDPROJ TO WS-IDPROJ                                    
158000                             W-IDPROJ                                     
158100        END-IF                                                            
158200                                                                          
158300        IF MID-KDPRODSL = ALL '+'                                         
158400           MOVE ARTG01-ART-KDPRODSL   TO W-KDPRODSL-1131                  
158500        ELSE                                                              
158600          MOVE MID-KDPRODSL           TO WS-KDPRODSL                      
158700                                         W-KDPRODSL-1131                  
158800        END-IF                                                            
158900                                                                          
159000        MOVE ARTG01-ART-IDPROJOBJ     TO W-IDPROJOBJ                      
159100        MOVE ARTG01-ART-IDPROJK       TO W-IDPROJK                        
159200        PERFORM IMS-GET-XXAQ11                                            
159300        IF SEGMENT-FINNS                                                  
159400           IF XXAQ-1132-TIFINLEV = ZERO                                   
159500              CONTINUE                                                    
159600           ELSE                                                           
159700             MOVE 'AAMMDD'           TO DAT-KDDATFORM                     
159800             MOVE XXAQ-1132-TIFINLEV TO DAT-I-TIDATUM                     
159900             CALL WDATKONV USING DAT-KDDATFORM                            
160000                                 DAT-I-TIDATUM                            
160100                                 DAT-O-TIDATUM                            
160200                                 DAT-KDSVAR                               
160300             IF DAT-KDSVAR-OK                                             
160400                MOVE DAT-TIAAVVD        TO WS-TIFINLV                     
160500             ELSE                                                         
160600                MOVE ZERO               TO WS-TIFINLV                     
160700             END-IF                                                       
160800           END-IF                                                         
160900        END-IF                                                            
161000                                                                          
161100        IF MID-IDRITN = ALL '+'                                           
161200           CONTINUE                                                       
161300        ELSE                                                              
161400          MOVE MID-IDRITN TO WS-IDRITN                                    
161500        END-IF                                                            
161600                                                                          
161700        IF MID-IDPROENH-1 = ALL '+'                                       
161800           CONTINUE                                                       
161900        ELSE                                                              
162000           MOVE MID-IDPROENH-1 TO WS-IDPROENH(1)                          
162100        END-IF                                                            
162200                                                                          
162300        MOVE MID-IDPROENH-2 TO WS-IDPROENH(2)                             
162400        MOVE MID-IDPROENH-3 TO WS-IDPROENH(3)                             
162500                                                                          
162600        IF MID-IDBERED = ALL '+'                                          
162700           CONTINUE                                                       
162800        ELSE                                                              
162900           MOVE MID-IDBERED TO WS-IDBERED                                 
163000        END-IF                                                            
163100                                                                          
163200        IF MID-KDSORT = ALL '+'                                           
163300           CONTINUE                                                       
163400        ELSE                                                              
163500           MOVE MID-KDSORT TO  WS-KDSORT                                  
163600        END-IF                                                            
163700                                                                          
163800        IF MID-IDAO = ALL '+'                                             
163900           CONTINUE                                                       
164000        ELSE                                                              
164100          MOVE MID-IDAO TO WS-IDAO                                        
164200        END-IF                                                            
164300                                                                          
164400        MOVE MID-TISOP   TO XX-TISOP                                      
164500        MOVE '+' TO XX-DAG-SOP                                            
164600        IF XX-TISOP   = ALL '+'                                           
164700           CONTINUE                                                       
164800        ELSE                                                              
164900           MOVE MID-TISOP   TO XX-TISOP                                   
165000                                                                          
165100           IF  XX-AAR-SOP = '99'                                          
165200           AND XX-VECKA-SOP = '99'                                        
165300              MOVE '9' TO XX-DAG-SOP                                      
165400           ELSE                                                           
165500              MOVE '1' TO XX-DAG-SOP                                      
165600           END-IF                                                         
165700                                                                          
165800           MOVE XX-TISOP   TO WS-TISOP                                    
165900           PERFORM S25-TIFINLV-FRAN-SOP                                   
166000        END-IF                                                            
166100                                                                          
166200        IF MID-IDFKNGRP = ALL '+'                                         
166300           CONTINUE                                                       
166400        ELSE                                                              
166500           MOVE MID-IDFKNGRP TO WS-IDFKNGRP                               
166600        END-IF                                                            
166700                                                                          
166800        IF MID-TEARTNOT-2 = ALL '+'                                       
166900           CONTINUE                                                       
167000        ELSE                                                              
167100           MOVE MID-TEARTNOT-2 TO WS-TEARTNOT-2                           
167200        END-IF                                                            
167300                                                                          
167400     ELSE                                                                 
167500                                                                          
167600        IF MID-IDRITN = ALL '+'                                           
167700          MOVE SPACE TO WS-IDRITN                                         
167800        ELSE                                                              
167900          MOVE MID-IDRITN TO WS-IDRITN                                    
168000        END-IF                                                            
168100                                                                          
168200        IF MID-IDPROJ = ALL '+'                                           
168300          MOVE SPACE TO WS-IDPROJ                                         
168400        ELSE                                                              
168500          MOVE MID-IDPROJ TO WS-IDPROJ                                    
168600        END-IF                                                            
168700                                                                          
168800        IF MID-IDPROENH-1 = ALL '+'                                       
168900          MOVE SPACE TO WS-IDPROENH(1)                                    
169000        ELSE                                                              
169100          MOVE MID-IDPROENH-1 TO WS-IDPROENH(1)                           
169200        END-IF                                                            
169300                                                                          
169400        IF MID-IDPROENH-2 = ALL '+'                                       
169500          MOVE SPACE TO WS-IDPROENH(2)                                    
169600        ELSE                                                              
169700          MOVE MID-IDPROENH-2 TO WS-IDPROENH(2)                           
169800        END-IF                                                            
169900                                                                          
170000        IF MID-IDPROENH-3 = ALL '+'                                       
170100          MOVE SPACE TO WS-IDPROENH(3)                                    
170200        ELSE                                                              
170300          MOVE MID-IDPROENH-3 TO WS-IDPROENH(3)                           
170400        END-IF                                                            
170500                                                                          
170600        MOVE MID-IDBERED TO WS-IDBERED                                    
170700                                                                          
170800        MOVE MID-KDSORT TO  WS-KDSORT                                     
170900                                                                          
171000        IF MID-IDAO = ALL '+'                                             
171100          MOVE SPACE TO WS-IDAO                                           
171200        ELSE                                                              
171300          MOVE MID-IDAO TO WS-IDAO                                        
171400        END-IF                                                            
171500                                                                          
171600        MOVE MID-TISOP   TO XX-TISOP                                      
171700                                                                          
171800        IF  XX-AAR-SOP = '99'                                             
171900        AND XX-VECKA-SOP = '99'                                           
172000           MOVE '9' TO XX-DAG-SOP                                         
172100        ELSE                                                              
172200           MOVE '1' TO XX-DAG-SOP                                         
172300        END-IF                                                            
172400                                                                          
172500        MOVE XX-TISOP   TO WS-TISOP                                       
172600        PERFORM S25-TIFINLV-FRAN-SOP                                      
172700                                                                          
172800        MOVE MID-IDFKNGRP TO WS-IDFKNGRP                                  
172900                                                                          
173000        IF MID-TEARTNOT-2 = ALL '+'                                       
173100           MOVE SPACE TO WS-TEARTNOT-2                                    
173200        ELSE                                                              
173300           MOVE MID-TEARTNOT-2 TO WS-TEARTNOT-2                           
173400        END-IF                                                            
173500                                                                          
173600        MOVE MID-KDPRODSL            TO WS-KDPRODSL                       
173700                                                                          
173800     END-IF                                                               
173900                                                                          
174000     PERFORM S100-LAS-KDAGE                                               
174100                                                                          
174200     IF MID-IDLEVNR = ALL '+' AND MID-BELEV = ALL '+'                     
174300         MOVE SPACE TO WS-IDLEVNR                                         
174400                       WS-BELEV                                           
174500     ELSE                                                                 
174600         MOVE MID-IDLEVNR TO WS-IDLEVNR                                   
174700         MOVE MID-BELEV TO WS-BELEV                                       
174800     END-IF                                                               
174900     IF MID-IDLEVNR = '1002 '                                             
175000        IF MID-BELEV = ALL '+' OR SPACE                                   
175100           MOVE SPACE TO WS-IDLEVNR                                       
175200                         WS-BELEV                                         
175300        END-IF                                                            
175400     END-IF                                                               
175500                                                                          
175600     IF MID-FLGAMART = ALL '+'                                            
175700        MOVE NEJ TO WS-FLGAMART                                           
175800     ELSE                                                                 
175900        MOVE MID-FLGAMART TO WS-FLGAMART                                  
176000     END-IF                                                               
176100                                                                          
176200     IF MID-TEARTNOT-4 = ALL '+'                                          
176300       MOVE SPACE TO WS-TEARTNOT-4                                        
176400     ELSE                                                                 
176500       MOVE MID-TEARTNOT-4 TO WS-TEARTNOT-4                               
176600     END-IF                                                               
176700                                                                          
176800     IF MID-TEARTNOT-7 = ALL '+'                                          
176900       MOVE SPACE TO WS-TEARTNOT-7                                        
177000     ELSE                                                                 
177100       MOVE MID-TEARTNOT-7 TO WS-TEARTNOT-7                               
177200     END-IF                                                               
177300                                                                          
177400     IF MID-IDKAT-1 = ALL '+'                                             
177500        MOVE SPACE TO WS-IDKAT(1)                                         
177600     ELSE                                                                 
177700        MOVE MID-IDKAT-1 TO WS-IDKAT(1)                                   
177800     END-IF                                                               
177900                                                                          
178000     IF MID-IDKAT-2 = ALL '+'                                             
178100        MOVE SPACE TO WS-IDKAT(2)                                         
178200     ELSE                                                                 
178300        MOVE MID-IDKAT-2 TO WS-IDKAT(2)                                   
178400     END-IF                                                               
178500                                                                          
178600     IF MID-IDKAT-3 = ALL '+'                                             
178700        MOVE SPACE TO WS-IDKAT(3)                                         
178800     ELSE                                                                 
178900        MOVE MID-IDKAT-3 TO WS-IDKAT(3)                                   
179000     END-IF                                                               
179100                                                                          
179200     IF MID-IDPROJUP = ALL '+'                                            
179300       MOVE SPACE TO WS-IDPROJUP                                          
179400     ELSE                                                                 
179500       MOVE MID-IDPROJUP TO WS-IDPROJUP                                   
179600     END-IF                                                               
179700                                                                          
179800     IF MID-KDUART = ALL '+'                                              
179900       MOVE SPACE TO WS-KDUART                                            
180000     ELSE                                                                 
180100       MOVE MID-KDUART TO WS-KDUART                                       
180200     END-IF                                                               
180300                                                                          
180400     MOVE MID-KDYTBEH TO WS-KDYTBEH                                       
180500                                                                          
180600     MOVE MID-KDFARLIG TO WS-KDFARLIG                                     
180700                                                                          
180800     MOVE MID-KDBPSR TO WS-KDBPSR                                         
180900                                                                          
181000     MOVE MID-FLLSRDEL TO WS-FLLSRDEL                                     
181100     .                                                                    
181200     EJECT                                                                
181300 S02-UPPDATERA-ARTREG SECTION.                                            
181400     SKIP2                                                                
181500     MOVE IDARTNR-WS TO RESK-IDART                                        
181600     CALL W009KSIF USING RESK-IDART RESK-9-POS RESK-W009KSIFR             
181700                                                                          
181800*** WDK601                                                                
181900                                                                          
182000     MOVE 'IDAG  '   TO DAT-KDDATFORM                                     
182100     CALL WDATKONV USING DAT-KDDATFORM                                    
182200                         DAT-I-TIDATUM                                    
182300                         DAT-O-TIDATUM                                    
182400                         DAT-KDSVAR                                       
182500                                                                          
182600     IF DAT-KDSVAR-OK                                                     
182700        MOVE DAT-TIAAVVD    TO AAVVD                                      
182800        MOVE AAVV           TO W009VADD-DATUM                             
182900        MOVE +6             TO W009VADD-ANTAL                             
183000        CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                 
183100     ELSE                                                                 
183200        MOVE ZERO           TO W009VADD-DATUM                             
183300     END-IF                                                               
183400                                                                          
183500     MOVE IDARTNR-WS TO W-IDARTNR                                         
183600     PERFORM IMS-GET-ARTC01                                               
183700                                                                          
183800     MOVE IDARTNR-WS            TO ART-IDARTNR                            
183900                                                                          
184000     MOVE RESK-W009KSIFR         TO ART-REKSIFFR                          
184100                                                                          
184200     IF WS-FLGAMART = JA                                                  
184300       MOVE WS-FLERS            TO ART-FLERS                              
184400     ELSE                                                                 
184500       MOVE NEJ                 TO ART-FLERS                              
184600     END-IF                                                               
184700                                                                          
184800     MOVE ZERO                  TO ART-KDERS-UTG                          
184900                                   ART-TIURPROD                           
184910                                   ART-KDANSKSEG                          
185000     MOVE ZERO                  TO ART-TIERSDAT                           
185100                                                                          
185200     IF  WS-FLGAMART = JA                                                 
185300        PERFORM S012-UPDATE-WDGX2264-2266                                 
185400     END-IF                                                               
185500                                                                          
185501     MOVE MID-IDCDS             TO ART-IDCDS                              
185502     MOVE MID-KDARTSYS          TO ART-KDARTSYS                           
185503                                                                          
185600     MOVE WS-TISOP              TO ART-TISOP                              
185700     MOVE SPAR-TIFINLV-AAVVD    TO ART-TIFINLV                            
185800     IF MID-IDLEVNR = '1002 '                                             
185900        MOVE MID-IDLEVNR        TO ART-IDLEVNR                            
186000     ELSE                                                                 
186100        MOVE SPACE              TO ART-IDLEVNR                            
186200     END-IF                                                               
186300     MOVE DAGENS-DATUM          TO ART-TIREGDAT                           
186400     MOVE NEJ                   TO ART-FLIART                             
186500     MOVE WS-IDFKNGRP           TO ART-IDFKNGRP                           
186600     MOVE WS-KDPRODSL           TO ART-KDPRODSL                           
186700     MOVE WS-KDSORT             TO ART-KDSORT                             
186800                                                                          
186900     PERFORM S98-HAEMTA-IDFTG                                             
187000     MOVE KPS-IDFTG             TO ART-IDFTG                              
187100     PERFORM S021-LAGG-UPP-IDAO                                           
187200                                                                          
187300     MOVE ART-TIFINLV             TO WS-TIFINLV                           
187400     MOVE WS-TIFINLV              TO MOD-MOD-TIFINLV-UT                   
187500     MOVE ART-TISOP               TO WS-TISOP                             
187600     MOVE WS-TISOP                TO MOD-MOD-TISOP-UT                     
187700     MOVE ART-IDAO(1)             TO MOD-MOD-IDAO-UT                      
187800     MOVE ART-IDFKNGRP            TO WS-IDFKNGRP                          
187900     MOVE WS-IDFKNGRP             TO MOD-MOD-IDFKNGRP-UT                  
188000     MOVE ART-KDPRODSL            TO WS-KDPRODSL                          
188100     MOVE WS-KDPRODSL             TO MOD-MOD-KDPRODSL-UT                  
188200     MOVE ART-KDSORT              TO MOD-MOD-KDSORT-UT                    
188300                                                                          
188400     IF WS-FLGAMART = JA                                                  
188500       PERFORM IMS-REPL-ARTC                                              
188600     ELSE                                                                 
188601                                                                          
188610       MOVE 'N'                     TO ART-FLBRAND                        
188620       MOVE 'N'                     TO ART-FLBSNES                        
188630       MOVE  0                      TO ART-KDSOP                          
188640       MOVE 15                      TO ART-KVEOP                          
188650                                                                          
188660*** CALL W221SEGM TO SET KDANSKSEG                                        
188670       MOVE 001                 TO SEGM-KDCALL                            
188680       MOVE ART-IDARTNR         TO SEGM-IDARTNR                           
188690       MOVE ZERO                TO SEGM-KDANSKSEG-IN                      
188691       MOVE SPACE               TO SEGM-IDDC                              
188692       MOVE SPACE               TO SEGM-IDREFTAB-IN                       
188693       MOVE WS-KDPRODSL         TO SEGM-KDPRODSL                          
188694       MOVE ART-FLBSNES         TO SEGM-FLBSNES                           
188695       MOVE ART-KVEOP           TO SEGM-KVEOP                             
188696       MOVE ZERO                TO SEGM-KDVSOP                            
188697       MOVE ART-TISOP           TO SEGM-TISOP                             
188698       MOVE ZERO                TO SEGM-TIURPROD                          
188699       MOVE WS-KDFARLIG         TO SEGM-KDFARLIG                          
188700       CALL W221SEGM USING SEGM-W221SEGM                                  
188701                           SEGM-WDB6-PCB                                  
188702                           SEGM-WDL7-PCB                                  
188703                           SEGM-WDL8-PCB                                  
188704                           SEGM-WDD5-PCB                                  
188705       IF  SEGM-KDSVAR-OK                                                 
188706           IF SEGM-FLANSKSEG-CHANGED = JA                                 
188707              MOVE SEGM-KDANSKSEG    TO ART-KDANSKSEG                     
188708           ELSE                                                           
188709              MOVE SEGM-KDANSKSEG-IN TO ART-KDANSKSEG                     
188710           END-IF                                                         
188712       ELSE                                                               
188713           DISPLAY 'W221SEGM-ERROR1:' SEGM-TEXT                           
188714           CALL FELLOG                                                    
188715       END-IF                                                             
188716                                                                          
188720       PERFORM IMS-ISRT-ARTC01                                            
188800     END-IF                                                               
188900                                                                          
189000*** WDK611                                                                
189100                                                                          
189200     MOVE MFS-RENSA-FAELT      TO MOD-MOD-IDPROENH-1-UT                   
189300                                  MOD-MOD-IDPROENH-2-UT                   
189400                                  MOD-MOD-IDPROENH-3-UT                   
189500     PERFORM IMS-GET-ARTC01                                               
189600     PERFORM IMS-GET-ARTC11                                               
189700                                                                          
189800     IF MID-IDLEVNR = '1002 '                                             
189900        MOVE MID-IDLEVNR        TO CLAG-IDLEVNR-SHIP                      
190000     ELSE                                                                 
190100        MOVE SPACE              TO CLAG-IDLEVNR-SHIP                      
190200     END-IF                                                               
190300     MOVE '1'                   TO CLAG-KDSEGKEY                          
190400     MOVE ZERO                  TO CLAG-ADLAGOMR                          
190500     MOVE ZERO                  TO CLAG-ADGANG                            
190600     MOVE ZERO                  TO CLAG-ADPLATS                           
190700     MOVE ZERO                  TO CLAG-ADLAGOMR-SVS                      
190800     MOVE ZERO                  TO CLAG-ADGANG-SVS                        
190900     MOVE ZERO                  TO CLAG-ADPLATS-SVS                       
191000     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(1)                    
191100     MOVE ZERO                  TO CLAG-ADGANG-CD(1)                      
191200     MOVE ZERO                  TO CLAG-ADPLATS-CD(1)                     
191300     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(2)                    
191400     MOVE ZERO                  TO CLAG-ADGANG-CD(2)                      
191500     MOVE ZERO                  TO CLAG-ADPLATS-CD(2)                     
191600     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(3)                    
191700     MOVE ZERO                  TO CLAG-ADGANG-CD(3)                      
191800     MOVE ZERO                  TO CLAG-ADPLATS-CD(3)                     
191900     MOVE ZERO                  TO CLAG-ADLAGOMR-CD(4)                    
192000     MOVE ZERO                  TO CLAG-ADGANG-CD(4)                      
192100     MOVE ZERO                  TO CLAG-ADPLATS-CD(4)                     
192200     MOVE SPACE                 TO CLAG-ADINPORT                          
192300     MOVE ZERO                  TO CLAG-KDEXCHA                           
192400     MOVE ZERO                  TO CLAG-KVPOINT                           
192500     MOVE ZERO                  TO CLAG-DAXPOINT                          
192600     MOVE ZERO                  TO CLAG-PRREF                             
192700     MOVE SPACE                 TO CLAG-IDUSER-VUPD                       
192800     MOVE 'N'                   TO CLAG-FLMANOSK                          
192900     MOVE 'N'                   TO CLAG-FLFSP                             
193000     MOVE 'N'                   TO CLAG-FLAVRART                          
193100     MOVE 'N'                   TO CLAG-FLGEMART                          
193200     MOVE 'N'                   TO CLAG-FLMANAT                           
193300     MOVE 'N'                   TO CLAG-FLMANBK                           
193400     MOVE 'N'                   TO CLAG-FLMANKP                           
193500     MOVE 'N'                   TO CLAG-FLMANLT                           
193600     MOVE 'N'                   TO CLAG-FLMANQ                            
193700     MOVE 'N'                   TO CLAG-FLJIT                             
193800     MOVE 'N'                   TO CLAG-FLLARM-BUF                        
193900     MOVE 'N'                   TO CLAG-FLMPB                             
194000     MOVE 'N'                   TO CLAG-FLOREGPB                          
194100     MOVE 'N'                   TO CLAG-FLSKROT-BEORD                     
194200     MOVE 'N'                   TO CLAG-FLMANPB                           
194300     MOVE 'N'                   TO CLAG-FLSPKOST                          
194400     MOVE WS-FLLSRDEL           TO CLAG-FLLSRDEL                          
194500     MOVE 'N'                   TO CLAG-FLLTKSP                           
194600     MOVE 'N'                   TO CLAG-FLMANGK                           
194700     MOVE 'N'                   TO CLAG-FLTOPP                            
194800     MOVE 'N'                   TO CLAG-FLMANOPP                          
194900     MOVE 'N'                   TO CLAG-KDOPPLAN                          
195000     MOVE 'N'                   TO CLAG-FLRADREF                          
195100     MOVE 'N'                   TO CLAG-FLMARKSP                          
195200     MOVE 'N'                   TO CLAG-FLEJBUFF                          
195300     MOVE 'N'                   TO CLAG-FLTPO1                            
195400                                   CLAG-FLRELSP                           
195500     MOVE 'N'                   TO CLAG-FLCDART                           
195600     MOVE 'N'                   TO CLAG-FLNYBER                           
195700     MOVE 'N'                   TO CLAG-FLSKROT-AUTO                      
195800     MOVE 'N'                   TO CLAG-FLSKROT-BEV                       
195900     MOVE 'N'                   TO CLAG-FLSKROT-SL                        
196000     MOVE 'N'                   TO CLAG-FLSKROT-WLC                       
196010     MOVE 'N'                   TO CLAG-FLAUTREL                          
196100     MOVE ZERO                  TO CLAG-KDUVKNTO                          
196200     MOVE WS-KDPRODSL           TO TEST-KDPRODSL                          
196300     IF KDPRODSL-VOLVO-BIMA                                               
196400        MOVE 'J'                TO CLAG-FLNYBER                           
196500     END-IF                                                               
196600                                                                          
196700     PERFORM S09-KOLLA-FLREFILL                                           
196800     PERFORM S10-KOLLA-KDPSLLOC                                           
196900                                                                          
197000     MOVE WS-IDBERED            TO CLAG-IDBERED                           
197100     MOVE WS-IDPROJ             TO CLAG-IDPROJ                            
197200     MOVE WS-IDPROJUP           TO CLAG-IDPROJUP                          
197300     IF WS-IDRITN = '='                                                   
197400*****  KOPIERA ARTIKELNR TILL RITNINGSNR                                  
197500*****  ("KAPA AV" INLEDANDE NOLLOR)                                       
197600                                                                          
197700       MOVE WS-IDARTNR TO WS-W009REDU-IN                                  
197800       INSPECT WS-W009REDU-IN REPLACING LEADING ZERO BY SPACE             
197900       CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT                  
198000       MOVE WS-W009REDU-UT      TO CLAG-IDRITN                            
198100     ELSE                                                                 
198200       MOVE WS-IDRITN           TO CLAG-IDRITN                            
198300     END-IF                                                               
198400     IF WS-IDKAT(1) NOT = SPACE                                           
198500       MOVE WS-IDKAT(1)         TO CLAG-IDKAT(1)                          
198600       MOVE WS-IDKAT(2)         TO CLAG-IDKAT(2)                          
198700       MOVE WS-IDKAT(3)         TO CLAG-IDKAT(3)                          
198800     ELSE                                                                 
198900       MOVE SPACE               TO CLAG-IDKAT(1)                          
199000       MOVE SPACE               TO CLAG-IDKAT(2)                          
199100       MOVE SPACE               TO CLAG-IDKAT(3)                          
199200     END-IF                                                               
199300     MOVE ZERO                  TO CLAG-IDPSN                             
199400     MOVE ZERO                  TO CLAG-IDANSK                            
199500     MOVE SPACE                 TO CLAG-IDINK                             
199600     MOVE ZERO                  TO CLAG-IDPLANGR-AG                       
199700     MOVE ZERO                  TO CLAG-IDPLANGR-LEV                      
199800     MOVE SPACE                 TO CLAG-IDFS-SEN                          
199900                                   CLAG-IDLEVNR-SEN                       
200000                                   CLAG-IDDC-REF                          
200100     MOVE ZERO                  TO CLAG-IDLKTO                            
200200                                   CLAG-PRHEMTAG                          
200300     MOVE ZERO                  TO CLAG-IDSTATNR(1)                       
200400                                   CLAG-IDSTATNR(2)                       
200500                                   CLAG-IDSTATNR(3)                       
200600                                   CLAG-IDSTATNR(4)                       
200700                                   CLAG-IDSTATNR(5)                       
200800                                   CLAG-IDSTATNR(6)                       
200900                                   CLAG-IDARTNR-EMBQ0                     
201000                                   CLAG-IDARTNR-EMBQ1                     
201100                                   CLAG-IDARTNR-EMBQ2                     
201200                                   CLAG-IDARTNR-EMBQ3                     
201300                                   CLAG-IDARTNR-EMBQ4                     
201400                                   CLAG-TIUPPDAT-EMB                      
201500                                   CLAG-TIGILTIG-PCOO                     
201600                                   CLAG-TISTODAT-LARM                     
201700     MOVE SPACE                 TO CLAG-IDUSER-SPKVAL                     
201800                                   CLAG-IDUSER-EMB                        
201900                                   CLAG-KDPCOO                            
202000                                                                          
202100     MOVE ZERO  TO CLAG-IDPROENH(1)                                       
202200                   CLAG-IDPROENH(2)                                       
202300                   CLAG-IDPROENH(3)                                       
202400     MOVE +1 TO IX                                                        
202500     PERFORM UNTIL IX > MAX-IX                                            
202600       IF WS-IDPROENH(IX) = ALL '+'  OR  SPACE                            
202700          CONTINUE                                                        
202800       ELSE                                                               
202900          MOVE WS-IDPROENH(IX) TO CLAG-IDPROENH(IX)                       
203000          IF IX = 1                                                       
203100             MOVE CLAG-IDPROENH(1) TO MOD-MOD-IDPROENH-1-UT               
203200             INSPECT MOD-MOD-IDPROENH-1-UT REPLACING                      
203300             LEADING ZERO BY SPACE                                        
203400          ELSE                                                            
203500             IF IX = 2                                                    
203600                MOVE CLAG-IDPROENH(2) TO MOD-MOD-IDPROENH-2-UT            
203700                INSPECT MOD-MOD-IDPROENH-2-UT REPLACING                   
203800                LEADING ZERO BY SPACE                                     
203900             ELSE                                                         
204000                IF IX = 3                                                 
204100                   MOVE CLAG-IDPROENH(3)                                  
204200                                      TO MOD-MOD-IDPROENH-3-UT            
204300                   INSPECT MOD-MOD-IDPROENH-3-UT REPLACING                
204400                   LEADING ZERO BY SPACE                                  
204500                END-IF                                                    
204600             END-IF                                                       
204700          END-IF                                                          
204800       END-IF                                                             
204900       ADD +1 TO IX                                                       
205000     END-PERFORM                                                          
205100                                                                          
205200     MOVE WS-KDYTBEH            TO CLAG-KDYTBEH                           
205300     MOVE ZERO                  TO CLAG-BEFT                              
205400                                                                          
205500     MOVE ZERO                  TO CLAG-KDAVT                             
205600     MOVE WS-KDAGE              TO CLAG-KDAGE                             
205700     MOVE ZERO                  TO CLAG-KDHF                              
205800     MOVE +2                    TO CLAG-KDKSP                             
205900     MOVE ZERO                  TO CLAG-KDLPSP                            
206000     MOVE WS-KDPSLLOC           TO CLAG-KDPSLLOC                          
206100     MOVE ZERO                  TO CLAG-KDVVKL                            
206200     MOVE +1                    TO CLAG-KDKG                              
206300     MOVE ZERO                  TO CLAG-KDTIPPR                           
206400     MOVE ZERO                  TO CLAG-KDVTH                             
206500     MOVE WS-KDFARLIG           TO CLAG-KDFARLIG                          
206600     MOVE WS-KDBPSR             TO CLAG-KDBPSR                            
206700     MOVE ZERO                  TO CLAG-KDVSOP                            
206800     MOVE +1                    TO CLAG-KDLTK                             
206900     MOVE ZERO                  TO CLAG-KDTULLRE                          
207000     MOVE WS-KDUART             TO CLAG-KDUART                            
207100     MOVE ZERO                  TO CLAG-KDGK                              
207200     MOVE ZERO                  TO CLAG-KDLEVSP                           
207300     MOVE SPACE                 TO CLAG-KDARTURS                          
207400     MOVE ZERO                  TO CLAG-KDSRA                             
207500     MOVE ZERO                  TO CLAG-KDFORP                            
207600     MOVE SPACE                 TO CLAG-KDEFFMAN                          
207700     MOVE ZERO                  TO CLAG-KDEMBKOD-0                        
207800     MOVE ZERO                  TO CLAG-KDEMBKOD-1                        
207900     MOVE ZERO                  TO CLAG-KDEMBKOD-2                        
208000     MOVE ZERO                  TO CLAG-KDERS                             
208100     MOVE ZERO                  TO CLAG-KDARTHNT                          
208200     MOVE ZERO                  TO CLAG-KDSPEEMB                          
208300                                   CLAG-KVSPARR-KVAL                      
208400     MOVE SPACE                 TO CLAG-KDFREKKL                          
208500     MOVE SPACE                 TO CLAG-KDPRISKL                          
208600     MOVE 'P'                   TO CLAG-KDLEVPLF                          
208700     MOVE ZERO                  TO CLAG-KVINVS                            
208800     MOVE ZERO                  TO CLAG-KVAP                              
208900     MOVE ZERO                  TO CLAG-KVBK                              
209000     MOVE ZERO                  TO CLAG-KVDAGAR-FFH                       
209100     MOVE ZERO                  TO CLAG-KVDAGAR-INLEV                     
209200     MOVE ZERO                  TO CLAG-KVDAGAR-TT                        
209300     MOVE ZERO                  TO CLAG-KVKP                              
209400     MOVE ZERO                  TO CLAG-KVLAAN                            
209500     MOVE ZERO                  TO CLAG-KVOVERF                           
209600     MOVE ZERO                  TO CLAG-KVPALL                            
209700     MOVE ZERO                  TO CLAG-KVQ                               
209800     MOVE ZERO                  TO CLAG-KVQ-JUST                          
209900     MOVE ZERO                  TO CLAG-KVSLUTKP                          
210000     MOVE ZERO                  TO CLAG-KVVECKOR-AT                       
210100     MOVE ZERO                  TO CLAG-KVVECKOR-BT                       
210200     MOVE ZERO                  TO CLAG-KVVECKOR-FT                       
210300     MOVE ZERO                  TO CLAG-KVVECKOR-LT                       
210400     MOVE ZERO                  TO CLAG-KVVORKO                           
210500     MOVE ZERO                  TO CLAG-KVAVIS-SEN                        
210600     MOVE ZERO                  TO CLAG-KVMAD-SEP                         
210700     MOVE ZERO                  TO CLAG-KVMAD-TOT                         
210800     MOVE ZERO                  TO CLAG-KVMP                              
210900     MOVE ZERO                  TO CLAG-KVPB-SATS                         
211000     MOVE ZERO                  TO CLAG-KVPB-SEP                          
211100     MOVE ZERO                  TO CLAG-KVPB-TPO                          
211200     MOVE ZERO                  TO CLAG-KVPB-VESL                         
211300     MOVE ZERO                  TO CLAG-KVPB-HIST                         
211400     MOVE ZERO                  TO CLAG-KVUTJFEL                          
211500     MOVE ZERO                  TO CLAG-KVQPACK-0                         
211600     MOVE ZERO                  TO CLAG-KVQPACK-1                         
211700     MOVE ZERO                  TO CLAG-KVQPACK-2                         
211800     MOVE ZERO                  TO CLAG-KVQPACK-3                         
211900     MOVE ZERO                  TO CLAG-KVQPACK-4                         
212000     MOVE 8                     TO CLAG-KVFRYSTI                          
212100     MOVE ZERO                  TO CLAG-KVAKS-CDC                         
212200     MOVE ZERO                  TO CLAG-KVAKS-PAV                         
212300     MOVE ZERO                  TO CLAG-KVAKS-T                           
212400     MOVE ZERO                  TO CLAG-KVEFRS                            
212500     MOVE ZERO                  TO CLAG-KVLS                              
212600     MOVE ZERO                  TO CLAG-KVLS-SVS                          
212700     MOVE ZERO                  TO CLAG-KVLS-CD(1)                        
212800     MOVE ZERO                  TO CLAG-KVLS-CD(2)                        
212900     MOVE ZERO                  TO CLAG-KVLS-CD(3)                        
213000     MOVE ZERO                  TO CLAG-KVLS-CD(4)                        
213100     MOVE ZERO                  TO CLAG-KVOI-OVR                          
213200     MOVE ZERO                  TO CLAG-KVRESS-CD(1)                      
213300     MOVE ZERO                  TO CLAG-KVRESS-CD(2)                      
213400     MOVE ZERO                  TO CLAG-KVRESS-CD(3)                      
213500     MOVE ZERO                  TO CLAG-KVRESS-CD(4)                      
213600     MOVE ZERO                  TO CLAG-KVRESS                            
213700     MOVE ZERO                  TO CLAG-KVRETUR                           
213800     MOVE ZERO                  TO CLAG-KVROS                             
213900     MOVE ZERO                  TO CLAG-KVSLAGER                          
214000     MOVE ZERO                  TO CLAG-KVSPANT                           
214100     MOVE ZERO                  TO CLAG-KVUTRS                            
214200     MOVE ZERO                  TO CLAG-PRORDSK                           
214300     MOVE ZERO                  TO CLAG-PRARTSJK                          
214400     MOVE ZERO                  TO CLAG-PRARTSTD                          
214500     MOVE SPAR-PRARTBTO-EXP     TO CLAG-PRARTBTO-EXP                      
214600     MOVE ZERO                  TO CLAG-PRDIRLON                          
214700     MOVE ZERO                  TO CLAG-PRDMTRL                           
214800     MOVE ZERO                  TO CLAG-PRINK                             
214900     MOVE ZERO                  TO CLAG-PROVRPAL                          
215000     MOVE ZERO                  TO CLAG-PRLFKST                           
215100     MOVE ZERO                  TO CLAG-REDIRLEV                          
215200     MOVE ZERO                  TO CLAG-RESLJUST                          
215300     MOVE ZERO                  TO CLAG-RVPROFEL                          
215400     MOVE ZERO                  TO CLAG-RVPROURS                          
215500     MOVE ZERO                  TO CLAG-RETULF                            
215600     MOVE ZERO                  TO CLAG-TIRODAT                           
215700     MOVE ZERO                  TO CLAG-TILTK                             
215800     MOVE ZERO                  TO CLAG-TIBESRPT                          
215900     MOVE W009VADD-DATUM        TO CLAG-TIBESRPT-PAAM                     
216000     MOVE ZERO                  TO CLAG-TILPSP                            
216100                                   CLAG-TIOMSPEC                          
216200                                   CLAG-TIQJUST                           
216300*****                              CLAG-TIURPROD                          
216400                                   CLAG-TISLUTKP                          
216500                                   CLAG-TIPBDAT                           
216600                                   CLAG-TISLJUST                          
216700                                   CLAG-TIINVDAT                          
216800                                   CLAG-TIDISPIN                          
216900                                   CLAG-TIAVIDAT-SEN                      
217000                                   CLAG-TIREFSTO                          
217100                                   CLAG-TISPARR-KVAL                      
217200                                   CLAG-TISTOREF                          
217300                                   CLAG-TIMAIL-KVAL                       
217400                                   CLAG-TISKROT                           
217500                                   CLAG-TIPBLOCK                          
217600                                   CLAG-VKART                             
217700                                   CLAG-VLARTNTO                          
217800                                   CLAG-VKART-NTO                         
217900                                   CLAG-KVEOQ                             
218000                                   CLAG-KVULOAD                           
218100                                   CLAG-KVSLAGER-OPT                      
218200                                   CLAG-KVVECKOR-LVAR                     
218300                                   CLAG-KVPB-TREND                        
218400                                   CLAG-KVVECKOR-TREND                    
218500                                   CLAG-TIDATUM-TREND                     
218600                                   CLAG-TISKROT-AUTO                      
218700                                   CLAG-TISKPREL                          
218800                                   CLAG-KVBEART                           
218900                                   CLAG-KVAVROP-TOT                       
219000                                   CLAG-KVREFOVL-TOT                      
219100                                   CLAG-KVRETUR-TOT                       
219200                                   CLAG-KVTILLG-TOT                       
219210                                   CLAG-TIUPPDAT-VUPD                     
219300****                                                                      
219400     MOVE ZERO                  TO CLAG-DAPBPLAN                          
219500                                   CLAG-DASEASON                          
219600                                   CLAG-KVPB-PLAN                         
219700                                   CLAG-KVREFBER-PLOCK                    
219800                                   CLAG-KVREFPKT-PLOCK                    
219900                                   CLAG-KVOI-PLOCK                        
220000                                   CLAG-KVMAXPL                           
220100                                   CLAG-KVPB-PLAN-JUST1                   
220200                                   CLAG-TIPBPLAN-JUST1-FOM                
220300                                   CLAG-TIPBPLAN-JUST1-TOM                
220400                                   CLAG-KVPB-PLAN-JUST2                   
220500                                   CLAG-TIPBPLAN-JUST2-FOM                
220600                                   CLAG-TIPBPLAN-JUST2-TOM                
220700                                                                          
220800     MOVE 1.00                  TO CLAG-RESEASON-PLAN(1)                  
220900                                   CLAG-RESEASON-PLAN(2)                  
221000                                   CLAG-RESEASON-PLAN(3)                  
221100                                   CLAG-RESEASON-PLAN(4)                  
221200                                   CLAG-RESEASON-PLAN(5)                  
221300                                   CLAG-RESEASON-PLAN(6)                  
221400                                   CLAG-RESEASON-PLAN(7)                  
221500                                   CLAG-RESEASON-PLAN(8)                  
221600                                   CLAG-RESEASON-PLAN(9)                  
221700                                   CLAG-RESEASON-PLAN(10)                 
221800                                   CLAG-RESEASON-PLAN(11)                 
221900                                   CLAG-RESEASON-PLAN(12)                 
222000                                                                          
222100     MOVE ZERO                  TO CLAG-TILEVDAG (1)                      
222200                                   CLAG-TILEVDAG (2)                      
222300                                   CLAG-TILEVDAG (3)                      
222400                                   CLAG-TILEVDAG (4)                      
222500                                   CLAG-TILEVDAG (5)                      
222600     MOVE SPACE                 TO CLAG-ADINLOMR-BOA                      
222700                                                                          
222800****                                                                      
222900                                                                          
223000     MOVE CLAG-IDBERED          TO WS-IDBERED                             
223100     MOVE WS-IDBERED            TO MOD-MOD-IDBERED-UT                     
223200     MOVE CLAG-IDPROJ           TO MOD-MOD-IDPROJ-UT                      
223300     MOVE CLAG-IDPROJUP         TO MOD-MOD-IDPROJUP-UT                    
223400     MOVE CLAG-IDRITN           TO MOD-MOD-IDRITN-UT                      
223500     MOVE CLAG-KDYTBEH          TO WS-KDYTBEH                             
223600     MOVE WS-KDYTBEH            TO MOD-MOD-KDYTBEH-UT                     
223700     MOVE CLAG-IDKAT(1)         TO MOD-MOD-IDKAT-1-UT                     
223800     MOVE CLAG-IDKAT(2)         TO MOD-MOD-IDKAT-2-UT                     
223900     MOVE CLAG-IDKAT(3)         TO MOD-MOD-IDKAT-3-UT                     
224000     MOVE CLAG-FLLSRDEL         TO MOD-MOD-FLLSRDEL-UT                    
224100     MOVE CLAG-KDUART           TO MOD-MOD-KDUART-UT                      
224200     MOVE CLAG-KDFARLIG         TO WS-KDFARLIG                            
224300     MOVE WS-KDFARLIG           TO MOD-MOD-KDFARLIG-UT                    
224400     MOVE CLAG-KDBPSR           TO WS-KDBPSR                              
224500     MOVE WS-KDBPSR             TO MOD-MOD-KDBPSR-UT                      
224600                                                                          
224700     IF SEGMENT-FINNS                                                     
224800        PERFORM IMS-REPL-ARTC                                             
224900     ELSE                                                                 
225000        PERFORM IMS-ISRT-ARTC11                                           
225100     END-IF                                                               
225200                                                                          
225300*** WDK625                                                                
225400                                                                          
225500     IF WS-TEARTNOT-2 NOT = SPACE                                         
225600        MOVE WS-TEARTNOT-2      TO NOT-TEARTNOT                           
225700        MOVE +3                 TO NOT-KDNOTTYP                           
225800        PERFORM IMS-ISRT-ARTC25                                           
225900     END-IF                                                               
226000                                                                          
226100     IF WS-TEARTNOT-4 NOT = SPACE                                         
226200        MOVE WS-TEARTNOT-4      TO NOT-TEARTNOT                           
226300        MOVE +1                 TO NOT-KDNOTTYP                           
226400        PERFORM IMS-ISRT-ARTC25                                           
226500     END-IF                                                               
226600                                                                          
226700     IF WS-TEARTNOT-7 NOT = SPACE                                         
226800        MOVE WS-TEARTNOT-7      TO NOT-TEARTNOT                           
226900        MOVE +7                 TO NOT-KDNOTTYP                           
227000        PERFORM IMS-ISRT-ARTC25                                           
227100     END-IF                                                               
227200                                                                          
227300     MOVE WS-TEARTNOT-2         TO MOD-MOD-TEARTNOT-2                     
227400     MOVE WS-TEARTNOT-4         TO MOD-MOD-TEARTNOT-4                     
227500     MOVE WS-TEARTNOT-7         TO MOD-MOD-TEARTNOT-7                     
227600     .                                                                    
227700     EJECT                                                                
227800 S021-LAGG-UPP-IDAO SECTION.                                              
227900     SKIP2                                                                
228000     IF WS-FLGAMART = NEJ                                                 
228100        MOVE WS-IDAO        TO ART-IDAO(1)                                
228200        MOVE SPACE          TO ART-IDAO(2)                                
228300                               ART-IDAO(3)                                
228400                               ART-IDAO(4)                                
228500                               ART-IDAO(5)                                
228600     ELSE                                                                 
228700        MOVE ART-IDAO(4)    TO ART-IDAO(5)                                
228800        MOVE ART-IDAO(3)    TO ART-IDAO(4)                                
228900        MOVE ART-IDAO(2)    TO ART-IDAO(3)                                
229000        MOVE ART-IDAO(1)    TO ART-IDAO(2)                                
229100        MOVE WS-IDAO        TO ART-IDAO(1)                                
229200     END-IF                                                               
229300     .                                                                    
229400     EJECT                                                                
229500 S012-UPDATE-WDGX2264-2266 SECTION.                                       
229600                                                                          
229610     IF ART-TISOP NOT = WS-TISOP                                          
229700        MOVE ART-TISOP                   TO W-TISOP-2264-O                
229800        PERFORM IMS-GHU-WDGX2264-OLD                                      
229900        IF SEGMENT-FINNS                                                  
230000           MOVE WS-IDARTNR               TO W-IDARTNR-2266-O-MIN          
230100                                            W-IDARTNR-2266-O-MAX          
230200           PERFORM IMS-GHNP-WDGX2266-OLD                                  
230300           IF SEGMENT-FINNS                                               
230400              MOVE WS-TISOP              TO NEW-2264-TISOP                
230500              PERFORM IMS-ISRT-WDGX2264-NEW                               
230600           END-IF                                                         
230700           PERFORM UNTIL SEGMENT-SAKNAS                                   
230800              MOVE OLD-2266-WDGX2266     TO NEW-2266-WDGX2266             
230900              PERFORM IMS-ISRT-WDGX2266-NEW                               
231000              PERFORM IMS-DLET-WDGX2266-OLD                               
231100                                                                          
231200              PERFORM IMS-GHNP-WDGX2266-OLD                               
231300           END-PERFORM                                                    
231400                                                                          
231500           PERFORM IMS-GHU-WDGX2264-OLD                                   
231600           IF SEGMENT-FINNS                                               
231700              PERFORM IMS-GNP-WDGX2266                                    
231800              IF SEGMENT-SAKNAS                                           
231900                 PERFORM IMS-GHU-WDGX2264-OLD                             
232000                 PERFORM IMS-DLET-WDGX2264-OLD                            
232100              END-IF                                                      
232200           END-IF                                                         
232210        END-IF                                                            
232300     END-IF                                                               
232400     .                                                                    
232500     EJECT                                                                
232600 S03-UPPDATERA-KDPFIL SECTION.                                            
232700     SKIP2                                                                
232800     MOVE WS-KDPRODSL TO TEST-KDPRODSL                                    
232900     IF KDPRODSL-VCBV                                                     
233000        CONTINUE                                                          
233100     ELSE                                                                 
233200        ACCEPT ZZAC-TIKLOCK FROM TIME                                     
233300        ACCEPT ZZAC-TIAAMMDD FROM DATE                                    
233400        ADD +1 TO W-IDLOGLOP                                              
233500        MOVE W-IDLOGLOP TO ZZAC-IDLOGLOP                                  
233600        MOVE 'RZU' TO KDP-IDPTYP                                          
233700        IF WS-KDBPSR = 8 AND WS-FLLSRDEL = NEJ                            
233800           MOVE '-' TO KDP-KDUART                                         
233900        ELSE                                                              
234000           MOVE 'R' TO KDP-KDUART                                         
234100        END-IF                                                            
234200        MOVE IDARTNR-WS TO KDP-IDARTNR                                    
234300        MOVE IDARTNR-WS TO W092-SORTBGP                                   
234400        MOVE KDP-W10111 TO ZZAC-LOGGPOST                                  
234500        MOVE W092-AREA TO ZZAC-SORTPOST                                   
234600        PERFORM IMS-ISRT-ZZAC                                             
234700     END-IF                                                               
234800     .                                                                    
234900     EJECT                                                                
235000 S04-UPPDAT-CROSSIND-LISTPOST SECTION.                                    
235100     SKIP2                                                                
235200     MOVE ALL '+' TO PROG-MID                                             
235300                                                                          
235400     MOVE SPACE TO PROG-MID-IDARTNR-UT                                    
235500                   PROG-MID-IDLEVNR-UT                                    
235600                   PROG-MID-BELEVART-UT                                   
235700                                                                          
235800     IF WS-BELEV = SPACE                                                  
235900     AND WS-IDLEVNR = SPACE                                               
236000        CONTINUE                                                          
236100     ELSE                                                                 
236200        IF KOPIERING = NEJ                                                
236300           MOVE IDARTNR-WS    TO PROG-MID-IDARTNR-IN                      
236400                                 PROG-MID-IDARTNR-UP                      
236500        ELSE                                                              
236600           MOVE IDARTNR-NY-WS TO PROG-MID-IDARTNR-IN                      
236700                                 PROG-MID-IDARTNR-UP                      
236800        END-IF                                                            
236900        MOVE +9               TO PROG-MID-KDFTAG-UP                       
237000        MOVE WS-IDLEVNR       TO PROG-MID-IDLEVNR-UP                      
237100        MOVE +1               TO PROG-MID-IDBENR-UP                       
237200        MOVE WS-BELEV         TO PROG-MID-BELEVART-UP                     
237300        MOVE 'J'              TO PROG-MID-FLTLVM-UP                       
237400*****   MOVE '*'              TO PROG-MID-KDCMD-UP                        
237500        PERFORM IMS-INSERT-ALTMSG                                         
237600                                                                          
237700        ACCEPT ZZAC-TIKLOCK FROM TIME                                     
237800        ACCEPT ZZAC-TIAAMMDD FROM DATE                                    
237900        ADD +1 TO W-IDLOGLOP                                              
238000        MOVE W-IDLOGLOP TO ZZAC-IDLOGLOP                                  
238100        MOVE 'RZV' TO RZV-IDPTYP                                          
238200        MOVE WS-IDLEVNR       TO RZV-IDLEVNR                              
238300        MOVE WS-BELEV         TO RZV-BELEV                                
238400        MOVE WS-IDBERED       TO RZV-IDBERED                              
238500        IF KOPIERING = NEJ                                                
238600           MOVE IDARTNR-WS    TO RZV-IDARTNR                              
238700                                 W092-SORTBGP                             
238800        ELSE                                                              
238900           MOVE IDARTNR-NY-WS TO RZV-IDARTNR                              
239000                                 W092-SORTBGP                             
239100        END-IF                                                            
239200        MOVE RZV-W1011102 TO ZZAC-LOGGPOST                                
239300        MOVE W092-AREA TO ZZAC-SORTPOST                                   
239400        PERFORM IMS-ISRT-ZZAC                                             
239500     END-IF                                                               
239600     .                                                                    
239700     EJECT                                                                
239800 S05-IDSKYLT-BEART-FLRSBEART SECTION.                                     
239900     SKIP2                                                                
240000     IF MID-IDSKYLT = ALL '+'                                             
240100        MOVE 'S  '                TO WS-IDSKYLT                           
240200                                     W-IDSKYLT                            
240300     ELSE                                                                 
240400        MOVE MID-IDSKYLT          TO WS-IDSKYLT                           
240500                                     W-IDSKYLT                            
240600     END-IF                                                               
240700                                                                          
240800     IF MID-BEART   = ALL '+'                                             
240900        IF KOPIERING = JA                                                 
241000           MOVE IDARTNR-WS           TO W-IDARTNR                         
241100           PERFORM IMS-GET-BENA11-CSEQ                                    
241200           MOVE BENA-TEXT-BEART      TO WS-BEART                          
241300        ELSE                                                              
241400           MOVE ARTG01-ART-BEART-SVE TO WS-BEART                          
241500        END-IF                                                            
241600     ELSE                                                                 
241700        MOVE MID-BEART            TO WS-BEART                             
241800     END-IF                                                               
241900                                                                          
242000     IF MID-FLRSBEART = ALL '+'                                           
242100        MOVE NEJ                  TO WS-FLRSBEART                         
242200     ELSE                                                                 
242300        MOVE MID-FLRSBEART        TO WS-FLRSBEART                         
242400     END-IF                                                               
242500     .                                                                    
242600     EJECT                                                                
242700 S06-UPPDATERA-BENREG SECTION.                                            
242800*****************************************************************         
242900*   ÄT NOV 92  VID IDSKYLT GB GODKÄNNES BARA NAMNLEX. ARTIKEL   *         
243000*              REGISTRERAS PÅ FÖRSTA FUNNA NAMNLEXBENÄMNING     *         
243100*              FLFELHOMO SÄTTS TILL NEJ                         *         
243200*              GÄLLER INTE VID KOPIERING AV BENÄMNING           *         
243300*   ÄT OKT 02  VID ALL FÖRÄNDRING AV NÅGOT SEGMENT PÅ WDD3,     *         
243400*              SKALL FLAENDR SÄTTAS TILL JA PÅ WDD301           *         
243500*              IFALL BERÖRD ARTIKEL TILLHÖR PRODSL = 11-29      *         
243600*              (FLAENDR NEJ-SÄTTS I W159D2) "NEVIS-RUTIN"       *         
243700*****************************************************************         
243800     SKIP2                                                                
243900     MOVE NEJ TO SAMMA-BEN                                                
244000****  FÖR ATT FÅ SAMMA HOMONYMKOD                                         
244100     IF KOPIERING = JA                                                    
244200        MOVE IDARTNR-WS TO W-IDARTNR                                      
244300        PERFORM IMS-GU-BENA01-CSEQ                                        
244400        MOVE BENA-BEN-IDBENNR TO W-IDBENNR                                
244500        MOVE WS-IDSKYLT TO W-IDSKYLT                                      
244600        PERFORM IMS-GNP-BENA11-CSEQ                                       
244700        IF BENA-TEXT-BEART = WS-BEART                                     
244800           MOVE JA TO SAMMA-BEN                                           
244900           PERFORM S062-UPPDATERA-UTAN-HOM                                
245000        END-IF                                                            
245100     END-IF                                                               
245200                                                                          
245300     IF SAMMA-BEN = NEJ                                                   
245400        MOVE WS-IDSKYLT TO W-IDSKYLT                                      
245500        MOVE WS-BEART TO W-BEART                                          
245600                                                                          
245700        IF WS-IDSKYLT = 'S  '                                             
245800           PERFORM IMS-GET-BENA01-ASEQ                                    
245900           PERFORM UNTIL SEGMENT-SAKNAS OR BENA-BEN-KDHOMONYM = 0         
246000              PERFORM IMS-GET-BENA01-ASEQ                                 
246100           END-PERFORM                                                    
246200                                                                          
246300           IF SEGMENT-FINNS                                               
246400             MOVE BENA-BEN-IDBENNR TO W-IDBENNR                           
246500********     OM BENÄMNING FINNS PÅ BENÄMNINGSREGISTRET                    
246600              IF WS-FLRSBEART = JA                                        
246700                 PERFORM S062-UPPDATERA-UTAN-HOM                          
246800              ELSE                                                        
246900                 PERFORM IMS-GET-BENA13-ASEQ                              
247000                                                                          
247100                 IF SEGMENT-FINNS                                         
247200**************     OM HOMONYMKOD FINNS                                    
247300                    MOVE BENA-HOM-TEHOMONYM TO WS-TEHOMONYM               
247400                    IF RS-BM-NAMN = 'BM-NAMN' OR 'RS-NAMN'                
247500                       PERFORM S062-UPPDATERA-UTAN-HOM                    
247600                    ELSE                                                  
247700                       PERFORM S063-UPPDATERA-MED-HOM                     
247800                    END-IF                                                
247900                 ELSE                                                     
248000**************  OM HOMONYMKOD SAKNAS                                      
248100                    PERFORM S062-UPPDATERA-UTAN-HOM                       
248200                 END-IF                                                   
248300              END-IF                                                      
248400           ELSE                                                           
248500              PERFORM S061-REGISTRERA-BENREG                              
248600           END-IF                                                         
248700        ELSE                                                              
248800                                                                          
248900***********  VID IDSKYLT GB                                               
249000                                                                          
249100           PERFORM IMS-GET-BENA01-ASEQ                                    
249200           PERFORM UNTIL SEGMENT-SAKNAS OR BENA-BEN-KDBENSTAT < 2         
249300              PERFORM IMS-GET-BENA01-ASEQ                                 
249400           END-PERFORM                                                    
249500                                                                          
249600           IF SEGMENT-FINNS                                               
249700              MOVE BENA-BEN-IDBENNR TO W-IDBENNR                          
249800              PERFORM S062-UPPDATERA-UTAN-HOM                             
249900           END-IF                                                         
250000        END-IF                                                            
250100     END-IF                                                               
250200     .                                                                    
250300     EJECT                                                                
250400 S061-REGISTRERA-BENREG SECTION.                                          
250500     SKIP2                                                                
250600     PERFORM IMS-GU-XXAI01                                                
250700     PERFORM IMS-GHNP-XXAI11                                              
250800     ADD +1 TO XXAI-1208-IDBENNR                                          
250900     IF XXAI-1208-IDBENNR > XXAI-1208-IDBENNR-MAX                         
251000       MOVE +1 TO XXAI-1208-IDBENNR                                       
251100     END-IF                                                               
251200     MOVE XXAI-1208-IDBENNR  TO W-IDBENNR                                 
251300     PERFORM IMS-REPL-XXAI11                                              
251400                                                                          
251500     MOVE XXAI-1208-IDBENNR TO BENA-BEN-IDBENNR                           
251600     MOVE ZERO              TO BENA-BEN-KDHOMONYM                         
251700     MOVE ZERO              TO BENA-BEN-TIUPPDAT-STOP                     
251800     MOVE ZERO              TO BENA-BEN-KDBENSTAT                         
251900     MOVE WS-KDPRODSL       TO TEST-KDPRODSL                              
252000     IF KDPRODSL-VOLVO-ALL                                                
252100*     -- DET ÄR EN NEVIS-ARTIKEL                                          
252200       MOVE JA           TO BENA-BEN-FLAENDR                              
252300     END-IF                                                               
252400     PERFORM IMS-ISRT-BENA01                                              
252500                                                                          
252600     MOVE +1 TO TAB-IX                                                    
252700     PERFORM UNTIL TAB-IX > MAX-TAB-IDSKYLT                               
252800       MOVE TAB-IDSKYLT(TAB-IX) TO BENA-TEXT-IDSKYLT                      
252900       MOVE SPACE          TO BENA-TEXT-BEARTEXT                          
253000       MOVE DAGENS-DATUM   TO BENA-TEXT-TIUPPDAT                          
253100       IF BENA-TEXT-IDSKYLT = 'S  '                                       
253200          MOVE JA             TO BENA-TEXT-FLOVERSATT                     
253300          MOVE WS-BEART       TO BENA-TEXT-BEART                          
253400                                 WS-BEART-SVE                             
253500       ELSE                                                               
253600          MOVE NEJ            TO BENA-TEXT-FLOVERSATT                     
253700       END-IF                                                             
253800       PERFORM IMS-ISRT-BENA11                                            
253900       ADD +1 TO TAB-IX                                                   
254000     END-PERFORM                                                          
254100                                                                          
254200     MOVE ' BG'          TO BENA-TEXT-IDSKYLT                             
254300     MOVE SPACE          TO BENA-TEXT-BEARTEXT                            
254400     MOVE DAGENS-DATUM   TO BENA-TEXT-TIUPPDAT                            
254500     MOVE NEJ            TO BENA-TEXT-FLOVERSATT                          
254600     PERFORM IMS-ISRT-BENA11                                              
254700                                                                          
254800     MOVE SPACE TO REV-TETEXT                                             
254900     MOVE WS-BEART TO REV-TETEXT                                          
255000     CALL WREVERSE USING REV-TETEXT                                       
255100                                                                          
255200     MOVE '  S'          TO BENA-TEXT-IDSKYLT                             
255300     MOVE SPACE          TO BENA-TEXT-BEARTEXT                            
255400     MOVE REV-TETEXT     TO BENA-TEXT-BEART                               
255500     MOVE DAGENS-DATUM   TO BENA-TEXT-TIUPPDAT                            
255600     MOVE JA TO BENA-TEXT-FLOVERSATT                                      
255700     PERFORM IMS-ISRT-BENA11                                              
255800                                                                          
255900     IF KOPIERING = NEJ                                                   
256000       MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                
256100     ELSE                                                                 
256200       MOVE IDARTNR-NY-WS TO BENA-ART-IDARTNR                             
256300     END-IF                                                               
256400     MOVE NEJ TO BENA-ART-FLFELHOMO                                       
256500     PERFORM IMS-ISRT-BENA12                                              
256600                                                                          
256700     .                                                                    
256800     EJECT                                                                
256900 S062-UPPDATERA-UTAN-HOM SECTION.                                         
257000     SKIP2                                                                
257100     IF KOPIERING = NEJ                                                   
257200        MOVE IDARTNR-WS TO W-IDARTNR                                      
257300     ELSE                                                                 
257400        MOVE IDARTNR-NY-WS TO W-IDARTNR                                   
257500     END-IF                                                               
257600     PERFORM IMS-GET-BENA12                                               
257700     IF KOPIERING = NEJ                                                   
257800        MOVE IDARTNR-WS TO BENA-ART-IDARTNR                               
257900     ELSE                                                                 
258000        MOVE IDARTNR-NY-WS TO BENA-ART-IDARTNR                            
258100     END-IF                                                               
258200     MOVE NEJ        TO BENA-ART-FLFELHOMO                                
258300     PERFORM IMS-ISRT-BENA12                                              
258400                                                                          
258500*    --- FÖRÄNDRING HAR SKETT                                             
258600     MOVE WS-KDPRODSL       TO TEST-KDPRODSL                              
258700     IF KDPRODSL-VOLVO-ALL                                                
258800       PERFORM IMS-GET-BENA01                                             
258900*      -- DET ÄR EN NEVIS-ARTIKEL                                         
259000       MOVE JA TO BENA-BEN-FLAENDR                                        
259100       PERFORM IMS-REPL-BENA01                                            
259200     END-IF                                                               
259300                                                                          
259400     MOVE 'S  ' TO W-IDSKYLT                                              
259500     PERFORM IMS-GET-BENA11-CSEQ                                          
259600     MOVE BENA-TEXT-BEART TO WS-BEART-SVE                                 
259700     .                                                                    
259800     EJECT                                                                
259900 S063-UPPDATERA-MED-HOM SECTION.                                          
260000     SKIP2                                                                
260100     IF KOPIERING = NEJ                                                   
260200        MOVE IDARTNR-WS TO W-IDARTNR                                      
260300     ELSE                                                                 
260400        MOVE IDARTNR-NY-WS TO W-IDARTNR                                   
260500     END-IF                                                               
260600     PERFORM IMS-GET-BENA12                                               
260700     IF KOPIERING = NEJ                                                   
260800       MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                
260900     ELSE                                                                 
261000       MOVE IDARTNR-NY-WS TO BENA-ART-IDARTNR                             
261100     END-IF                                                               
261200     MOVE JA TO BENA-ART-FLFELHOMO                                        
261300     PERFORM IMS-ISRT-BENA12                                              
261400                                                                          
261500*    --- FÖRÄNDRING HAR SKETT                                             
261600     MOVE WS-KDPRODSL       TO TEST-KDPRODSL                              
261700     IF KDPRODSL-VOLVO-ALL                                                
261800       PERFORM IMS-GET-BENA01                                             
261900*      -- DET ÄR EN NEVIS-ARTIKEL                                         
262000       MOVE JA TO BENA-BEN-FLAENDR                                        
262100       PERFORM IMS-REPL-BENA01                                            
262200     END-IF                                                               
262300                                                                          
262400     MOVE 'S  ' TO W-IDSKYLT                                              
262500     PERFORM IMS-GET-BENA11-CSEQ                                          
262600     MOVE BENA-TEXT-BEART TO WS-BEART-SVE                                 
262700     .                                                                    
262800     EJECT                                                                
262900 S07-UPPDATERA-RASA SECTION.                                              
263000     SKIP2                                                                
263100     IF KOPIERING = NEJ                                                   
263200        MOVE IDARTNR-WS TO W-IDARTNR                                      
263300                           W-IDARTNR-S                                    
263400     ELSE                                                                 
263500        MOVE IDARTNR-NY-WS TO W-IDARTNR                                   
263600                              W-IDARTNR-S                                 
263700     END-IF                                                               
263800                                                                          
263900     PERFORM IMS-GET-SATB01                                               
264000     IF SEGMENT-FINNS                                                     
264100        MOVE SPACE TO      SATB-STR-BEART-SVE                             
264200        MOVE ZERO  TO      SATB-STR-KDBENHOM                              
264300                           SATB-STR-IDFKNGRP                              
264400                           SATB-STR-KDPRODSL                              
264500        PERFORM IMS-REPL-SATB                                             
264600     ELSE                                                                 
264700        IF WS-KDSORT = 'SA' OR 'TM'                                       
264800          PERFORM S99-NYUPPLAGG-RASA                                      
264900        END-IF                                                            
265000     END-IF                                                               
265100                                                                          
265200     MOVE SPACE      TO W-IDLEVNR-S                                       
265300                        W-BELEVART-S                                      
265400     PERFORM IMS-GET-SATB11-CSEQ                                          
265500     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
265600        MOVE SATE-STR-IDARTNR   TO W-IDARTNR-SATS                         
265700        MOVE SATE-RAD-KDSTRRAD  TO W-KDSTRRAD                             
265800        MOVE SATE-RAD-IDRADNR   TO W-IDRADNR                              
265900        PERFORM IMS-GHU-SATB11                                            
266000        IF SEGMENT-FINNS                                                  
266100           MOVE SPACE TO           SATB-RAD-BEART-SVE                     
266200                                   SATB-RAD-KDSORT                        
266300           MOVE ZERO  TO           SATB-RAD-KDBENHOM                      
266400           PERFORM IMS-REPL-SATB                                          
266500        END-IF                                                            
266600        PERFORM IMS-GET-SATB11-CSEQ                                       
266700     END-PERFORM                                                          
266800     .                                                                    
266900     EJECT                                                                
267000 S08-VAECKNING-RASA SECTION.                                              
267100     SKIP2                                                                
267200     MOVE IDARTNR-WS TO W-IDARTNR                                         
267300     PERFORM IMS-GET-SATB01                                               
267400     IF SEGMENT-FINNS                                                     
267500        MOVE SATB-STR-TIBORT TO WS-TIBORT                                 
267600        MOVE ZERO TO SATB-STR-TIBORT                                      
267700        PERFORM IMS-REPL-SATB                                             
267800        PERFORM IMS-GET-SATB11                                            
267900        PERFORM UNTIL SEGMENT-SAKNAS                                      
268000           IF SATB-RAD-TISTODAT = +999999                                 
268100              MOVE WS-TIBORT TO SATB-RAD-TISTODAT                         
268200              PERFORM IMS-REPL-SATB                                       
268300           END-IF                                                         
268400           PERFORM IMS-GET-SATB11                                         
268500        END-PERFORM                                                       
268600     ELSE                                                                 
268700        IF WS-KDSORT = 'SA' OR 'TM'                                       
268800          PERFORM S99-NYUPPLAGG-RASA                                      
268900        END-IF                                                            
269000     END-IF                                                               
269100     .                                                                    
269200     EJECT                                                                
269300 S09-KOLLA-FLREFILL SECTION.                                              
269400                                                                          
269500     MOVE 'J' TO CLAG-FLREFILL                                            
269600                                                                          
269700     IF KOPIERING = NEJ                                                   
269800        MOVE IDARTNR-WS TO BYT03-IDARTNR                                  
269900     ELSE                                                                 
270000        MOVE IDARTNR-NY-WS TO BYT03-IDARTNR                               
270100     END-IF                                                               
270200                                                                          
270300     MOVE WS-KDPRODSL   TO TEST-KDPRODSL                                  
270400     IF BYT03-OBJEKT                                                      
270500        MOVE 'N'        TO CLAG-FLREFILL                                  
270600     ELSE                                                                 
270700        IF KDPRODSL-BIMA                                                  
270800           MOVE 'N'     TO CLAG-FLREFILL                                  
270900        ELSE                                                              
271000           IF WS-KDPRODSL = 16 AND WS-IDFKNGRP = 3955                     
271100              MOVE 'N'  TO CLAG-FLREFILL                                  
271200           ELSE                                                           
271300              IF CLAG-FLLSRDEL = 'N'                                      
271400                 MOVE 'N' TO CLAG-FLREFILL                                
271500              ELSE                                                        
271600                 IF WS-KDSORT = 'SW'                                      
271700                    MOVE 'N' TO CLAG-FLREFILL                             
271800                 END-IF                                                   
271900              END-IF                                                      
272000           END-IF                                                         
272100        END-IF                                                            
272200     END-IF                                                               
272300     .                                                                    
272400     EJECT                                                                
272500 S10-KOLLA-KDPSLLOC SECTION.                                              
279640                                                                          
279641     IF KOPIERING = NEJ                                                   
279642        MOVE IDARTNR-WS              TO LPC-IDARTNR-IN                    
279643     ELSE                                                                 
279644        MOVE IDARTNR-NY-WS           TO LPC-IDARTNR-IN                    
279645     END-IF                                                               
279660     MOVE WS-IDFKNGRP                TO LPC-IDFKNGRP-IN                   
279670     MOVE WS-KDPRODSL                TO LPC-KDPRODSL-IN                   
279680     MOVE ZERO                       TO LPC-KDPSLLOC-UT                   
279690     CALL W100LPC  USING LPC-AREA                                         
279691                                                                          
279692     MOVE LPC-KDPSLLOC-UT            TO WS-KDPSLLOC                       
279700     .                                                                    
279800     EJECT                                                                
279900 S25-TIFINLV-FRAN-SOP SECTION.                                            
280000                                                                          
280100*-- TIFINLV BORTTAGET UR MID JAN. 2015                                    
280200*-- TIFINLV SKALL SÄTTAS = TISOP                                          
280300*-- MEN OM TISOP INTEÄR STÖRRE ÄN INNEVARANDE VECKA                       
280400*-- SKALL TISOP SÄTTAS TILL DAGENS VECKA + 1                              
280500                                                                          
280600     MOVE WS-TISOP             TO SPAR-TIFINLV-AAVV                       
280700                                                                          
280800     IF SPAR-TIFINLV-AAVV-R > SPAR-DAGENS-AAVV-R                          
280900        MOVE WS-TISOP          TO SPAR-TIFINLV-AAVVD                      
281000     ELSE                                                                 
281100        MOVE SPAR-DAGENS-AAVV-R(1:2)   TO WS-YY                           
281200        MOVE SPAR-DAGENS-AAVV-R(3:2)   TO WS-WW                           
281300        IF WS-WW < 52                                                     
281400            ADD      1 TO WS-WW                                           
281500        ELSE                                                              
281600            ADD      1 TO WS-YY                                           
281700            MOVE 01 TO WS-WW                                              
281800        END-IF                                                            
281900        MOVE         1 TO WS-D                                            
282000        MOVE WS-YYWWD          TO SPAR-TIFINLV-AAVVD                      
282100     END-IF                                                               
282200     .                                                                    
282300                                                                          
282400 S98-HAEMTA-IDFTG SECTION.                                                
282500                                                                          
282600     MOVE '002'       TO KPS-KDCALL                                       
282700     MOVE WS-KDPRODSL TO KPS-KDPRODSL                                     
282800     CALL WKPSKONV USING KPS-WKPSAREA                                     
282900     IF KPS-KDSVAR = 'F'                                                  
283000        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
283100     END-IF                                                               
283200     .                                                                    
283300     EJECT                                                                
283400 S99-NYUPPLAGG-RASA SECTION.                                              
283500     IF KOPIERING = NEJ                                                   
283600        MOVE IDARTNR-WS TO     SATB-STR-IDARTNR                           
283700     ELSE                                                                 
283800        MOVE IDARTNR-NY-WS TO  SATB-STR-IDARTNR                           
283900     END-IF                                                               
284000                                                                          
284100     MOVE SPACE              TO SATB-STR-BEART-SVE                        
284200                                SATB-STR-FLEXFORP                         
284300     MOVE NEJ                TO SATB-STR-FLFORPQ                          
284400     MOVE 'S'                TO SATB-STR-IDSTRTYP                         
284500     MOVE SPACE              TO SATB-STR-IDTSPEC                          
284600     MOVE MSG-SIGNON-USERID  TO SATB-STR-IDUSER                           
284700     MOVE SPACE              TO SATB-STR-TESTRNOT(1)                      
284800                                SATB-STR-TESTRNOT(2)                      
284900     MOVE ZERO               TO SATB-STR-IDFKNGRP                         
285000     IF MID-IDLEVNR = '1002 '                                             
285100        MOVE MID-IDLEVNR     TO SATB-STR-IDLEVNR                          
285200     ELSE                                                                 
285300        MOVE SPACE           TO SATB-STR-IDLEVNR                          
285400     END-IF                                                               
285500     MOVE ZERO               TO SATB-STR-KDBENHOM                         
285600                                SATB-STR-KDPRODSL                         
285700                                SATB-STR-TIBORT                           
285800                                SATB-STR-TIUPPDAT                         
285900                                SATB-STR-KVBYGMIN                         
286000     MOVE DAGENS-DATUM       TO SATB-STR-TIREGDAT                         
286100     PERFORM IMS-ISRT-SATB01                                              
286200     .                                                                    
286300     EJECT                                                                
286400                                                                          
286500 S100-LAS-KDAGE SECTION.                                                  
286600                                                                          
286700     MOVE WS-KDPRODSL                TO W-KDPRODSL-1131                   
286800                                                                          
286900     IF MID-IDPROJK = ALL '+'                                             
287000        MOVE SPACE                   TO WS-IDPROJK                        
287100        IF KOPIERING = NEJ                                                
287200           MOVE IDARTNR-WS TO W-IDARTNR                                   
287300        ELSE                                                              
287400           MOVE IDARTNR-NY-WS TO W-IDARTNR                                
287500        END-IF                                                            
287600        PERFORM IMS-GET-ARTG01                                            
287700        IF SEGMENT-FINNS                                                  
287800           MOVE ARTG01-ART-IDPROJK   TO WS-IDPROJK                        
287900        END-IF                                                            
288000     ELSE                                                                 
288100        MOVE MID-IDPROJK             TO WS-IDPROJK                        
288200     END-IF                                                               
288300                                                                          
288400     MOVE SPACE                      TO W-IDPROJOBJ                       
288500     MOVE WS-IDPROJ                  TO W-IDPROJ                          
288600     MOVE WS-IDPROJK                 TO W-IDPROJK                         
288700     PERFORM IMS-GU-XXAQ01                                                
288800     IF SEGMENT-FINNS                                                     
288900        PERFORM IMS-GNP-XXAQ11                                            
289000        PERFORM UNTIL SEGMENT-SAKNAS                                      
289100           IF XXAQ-1132-IDPROJ = W-IDPROJ                                 
289200              MOVE XXAQ-1132-KDAGE   TO WS-KDAGE                          
289300           END-IF                                                         
289400           PERFORM IMS-GNP-XXAQ11                                         
289500        END-PERFORM                                                       
289600     END-IF                                                               
289700     .                                                                    
289800     EJECT                                                                
289900* IMS SEKTIONER                                                           
290000     SKIP3                                                                
290100 IMS-GET-MSG SECTION.                                                     
290200     MOVE '  QC' TO GODK-STATUSKODER                                      
290300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
290400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
290500     PERFORM IMS-STATUSKONTROLL                                           
290600     .                                                                    
290700     SKIP3                                                                
290800 IMS-INSERT-MSG SECTION.                                                  
290900     IF ENGLISH-TEXT                                                      
291000        IF MFS-IDTRANS = '9410'                                           
291100           CONTINUE                                                       
291200        ELSE                                                              
291300           MOVE 'N' TO MFS-KDHUVOMR                                       
291400        END-IF                                                            
291500     END-IF                                                               
291600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
291700     MOVE SPACE TO GODK-STATUSKODER                                       
291800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
291900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
292000     PERFORM IMS-STATUSKONTROLL                                           
292100     .                                                                    
292200     EJECT                                                                
292300 IMS-INSERT-ALTMSG  SECTION.                                              
292400                                                                          
292500     MOVE SPACE TO GODK-STATUSKODER                                       
292600     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
292700     MOVE  ALT-STATUS-CODE TO STATUS-WS                                   
292800     PERFORM IMS-STATUSKONTROLL                                           
292900     .                                                                    
293000     SKIP2                                                                
293100 IMS-GET-ARTG01 SECTION.                                                  
293200     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
293300            DELIMITED BY SIZE INTO SSA1                                   
293400     MOVE '  GE' TO GODK-STATUSKODER                                      
293500     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA-2 SSA1                   
293600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
293700     PERFORM IMS-STATUSKONTROLL                                           
293800     .                                                                    
293900     SKIP2                                                                
294000 IMS-REPL-ARTG SECTION.                                                   
294100     MOVE '  '   TO GODK-STATUSKODER                                      
294200     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA-2                       
294300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
294400     PERFORM IMS-STATUSKONTROLL                                           
294500     .                                                                    
294600     EJECT                                                                
294700 IMS-GET-ARTC01 SECTION.                                                  
294800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
294900            DELIMITED BY SIZE INTO SSA1                                   
295000     MOVE '  GE' TO GODK-STATUSKODER                                      
295100     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
295200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
295300     PERFORM IMS-STATUSKONTROLL                                           
295400     .                                                                    
295500     SKIP2                                                                
295600 IMS-GHU-ARTC01 SECTION.                                                  
295700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
295800            DELIMITED BY SIZE INTO SSA1                                   
295900     MOVE '  ' TO GODK-STATUSKODER                                        
296000     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
296100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
296200     PERFORM IMS-STATUSKONTROLL                                           
296300     .                                                                    
296400     SKIP2                                                                
296500 IMS-GET-ARTC11-KOP SECTION.                                              
296600     MOVE 'WDK611 ' TO SSA1                                               
296700     MOVE '    ' TO GODK-STATUSKODER                                      
296800     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
296900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
297000     PERFORM IMS-STATUSKONTROLL                                           
297100     .                                                                    
297200     SKIP2                                                                
297300 IMS-GET-ARTC11 SECTION.                                                  
297400     MOVE 'WDK611 ' TO SSA1                                               
297500     MOVE '  GE' TO GODK-STATUSKODER                                      
297600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
297700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
297800     PERFORM IMS-STATUSKONTROLL                                           
297900     .                                                                    
298000     EJECT                                                                
298100 IMS-ISRT-ARTC01 SECTION.                                                 
298200     MOVE 'WDK601 ' TO SSA1                                               
298300     MOVE '  ' TO GODK-STATUSKODER                                        
298400     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA SSA1                    
298500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
298600     PERFORM IMS-STATUSKONTROLL                                           
298700     .                                                                    
298800     SKIP2                                                                
298900 IMS-ISRT-ARTC11 SECTION.                                                 
299000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
299100            DELIMITED BY SIZE INTO SSA1                                   
299200     MOVE 'WDK611 ' TO SSA2                                               
299300     MOVE '  ' TO GODK-STATUSKODER                                        
299400     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA SSA1 SSA2               
299500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
299600     PERFORM IMS-STATUSKONTROLL                                           
299700     .                                                                    
299800     SKIP2                                                                
299900 IMS-ISRT-ARTC25 SECTION.                                                 
300000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
300100            DELIMITED BY SIZE INTO SSA1                                   
300200     MOVE 'WDK611 ' TO SSA2                                               
300300     MOVE 'WDK625 ' TO SSA3                                               
300400     MOVE '  ' TO GODK-STATUSKODER                                        
300500     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA SSA1                    
300600                                    SSA2 SSA3                             
300700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
300800     PERFORM IMS-STATUSKONTROLL                                           
300900     .                                                                    
301000     EJECT                                                                
301100 IMS-REPL-ARTC SECTION.                                                   
301200     MOVE '  '   TO GODK-STATUSKODER                                      
301300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
301400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
301500     PERFORM IMS-STATUSKONTROLL                                           
301600     .                                                                    
301700     EJECT                                                                
301800 IMS-ISRT-ZZAC SECTION.                                                   
301900     MOVE 'WLZZAC01 ' TO SSA1                                             
302000     MOVE '  ' TO GODK-STATUSKODER                                        
302100     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
302200     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
302300     PERFORM IMS-STATUSKONTROLL                                           
302400     .                                                                    
302500     SKIP2                                                                
302600 IMS-GET-XXAQ11 SECTION.                                                  
302700     STRING 'WLXXAQ01(WDGXKEY  =' W-1131-KEY-X ')'                        
302800            DELIMITED BY SIZE INTO SSA1                                   
302900     STRING 'WLXXAQ11(WDGXKEY  =' W-1132-KEY-X ')'                        
303000            DELIMITED BY SIZE INTO SSA2                                   
303100     MOVE '  GE' TO GODK-STATUSKODER                                      
303200     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA SSA1 SSA2                 
303300     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
303400     PERFORM IMS-STATUSKONTROLL                                           
303500     .                                                                    
303600     EJECT                                                                
303700 IMS-GET-XXAT01 SECTION.                                                  
303800     STRING 'WLXXAT01(WDGXKEY  =' W-1137-KEY-X ')'                        
303900            DELIMITED BY SIZE INTO SSA1                                   
304000     MOVE '  GE' TO GODK-STATUSKODER                                      
304100     CALL CBLTDLI USING GU XXAT-PCB DLI-IO-AREA SSA1                      
304200     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
304300     PERFORM IMS-STATUSKONTROLL                                           
304400     .                                                                    
304500     SKIP2                                                                
304600 IMS-GET-XXAT11 SECTION.                                                  
304700     MOVE 'WLXXAT11 ' TO SSA1                                             
304800     MOVE '  GE' TO GODK-STATUSKODER                                      
304900     CALL CBLTDLI USING GNP XXAT-PCB DLI-IO-AREA SSA1                     
305000     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
305100     PERFORM IMS-STATUSKONTROLL                                           
305200     .                                                                    
305300     EJECT                                                                
305400 IMS-ISRT-XXBW SECTION.                                                   
305500     SKIP2                                                                
305600     STRING 'WLXXBW01(WDGXKEY  =' W-2227-KEY-X ')'                        
305700            DELIMITED BY SIZE INTO SSA1                                   
305800     MOVE 'WLXXBW11 ' TO SSA2                                             
305900     MOVE '  II' TO GODK-STATUSKODER                                      
306000     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA SSA1 SSA2               
306100     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
306200     PERFORM IMS-STATUSKONTROLL                                           
306300     .                                                                    
306400     SKIP2                                                                
306500 IMS-GET-BENA12 SECTION.                                                  
306600     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
306700                 DELIMITED BY SIZE INTO SSA1                              
306800     STRING 'WLBENA12(IDARTNR  =' W-IDARTNR-X ')'                         
306900                 DELIMITED BY SIZE INTO SSA2                              
307000     MOVE '  GE' TO GODK-STATUSKODER                                      
307100     CALL CBLTDLI USING GHU BENA-PCB DLI-IO-AREA     SSA1 SSA2            
307200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
307300     PERFORM IMS-STATUSKONTROLL                                           
307400     .                                                                    
307500     EJECT                                                                
307600 IMS-GET-BENA01-ASEQ SECTION.                                             
307700     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
307800                      W-BEART-X  ')'                                      
307900            DELIMITED BY SIZE INTO SSA1                                   
308000     MOVE '  GE' TO GODK-STATUSKODER                                      
308100     CALL CBLTDLI USING GN BENB-PCB DLI-IO-AREA     SSA1                  
308200     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
308300     PERFORM IMS-STATUSKONTROLL                                           
308400     .                                                                    
308500     SKIP2                                                                
308600 IMS-GET-BENA13-ASEQ SECTION.                                             
308700     MOVE 'WLBENA13 ' TO SSA1                                             
308800     MOVE '  GE' TO GODK-STATUSKODER                                      
308900     CALL CBLTDLI USING GNP BENB-PCB DLI-IO-AREA SSA1                     
309000     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
309100     PERFORM IMS-STATUSKONTROLL                                           
309200     .                                                                    
309300     EJECT                                                                
309400 IMS-GET-BENA11-CSEQ SECTION.                                             
309500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
309600            DELIMITED BY SIZE INTO SSA1                                   
309700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
309800            DELIMITED BY SIZE INTO SSA2                                   
309900     MOVE '  ' TO GODK-STATUSKODER                                        
310000     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1 SSA2                 
310100     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
310200     PERFORM IMS-STATUSKONTROLL                                           
310300     .                                                                    
310400     SKIP2                                                                
310500 IMS-GU-BENA01-CSEQ SECTION.                                              
310600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
310700            DELIMITED BY SIZE INTO SSA1                                   
310800     MOVE '  ' TO GODK-STATUSKODER                                        
310900     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1                      
311000     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
311100     PERFORM IMS-STATUSKONTROLL                                           
311200     .                                                                    
311300     SKIP2                                                                
311400 IMS-GNP-BENA11-CSEQ SECTION.                                             
311500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
311600            DELIMITED BY SIZE INTO SSA1                                   
311700     MOVE '  ' TO GODK-STATUSKODER                                        
311800     CALL CBLTDLI USING GNP BENC-PCB DLI-IO-AREA SSA1                     
311900     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
312000     PERFORM IMS-STATUSKONTROLL                                           
312100     .                                                                    
312200     EJECT                                                                
312300 IMS-ISRT-BENA01 SECTION.                                                 
312400     MOVE 'WLBENA01 ' TO SSA1                                             
312500     MOVE '  ' TO GODK-STATUSKODER                                        
312600     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1                    
312700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
312800     PERFORM IMS-STATUSKONTROLL                                           
312900     .                                                                    
313000     SKIP3                                                                
313100 IMS-GET-BENA01 SECTION.                                                  
313200     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
313300            DELIMITED BY SIZE INTO SSA1                                   
313400     MOVE '  ' TO GODK-STATUSKODER                                        
313500     CALL CBLTDLI USING GHU  BENA-PCB DLI-IO-AREA SSA1                    
313600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
313700     PERFORM IMS-STATUSKONTROLL                                           
313800     .                                                                    
313900     SKIP3                                                                
314000 IMS-REPL-BENA01 SECTION.                                                 
314100     MOVE '  ' TO GODK-STATUSKODER                                        
314200     CALL CBLTDLI USING REPL BENA-PCB DLI-IO-AREA                         
314300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
314400     PERFORM IMS-STATUSKONTROLL                                           
314500     .                                                                    
314600     SKIP2                                                                
314700 IMS-ISRT-BENA11 SECTION.                                                 
314800     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
314900            DELIMITED BY SIZE INTO SSA1                                   
315000     MOVE 'WLBENA11 ' TO SSA2                                             
315100     MOVE '  ' TO GODK-STATUSKODER                                        
315200     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
315300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
315400     PERFORM IMS-STATUSKONTROLL                                           
315500     .                                                                    
315600     SKIP3                                                                
315700 IMS-ISRT-BENA12 SECTION.                                                 
315800     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
315900            DELIMITED BY SIZE INTO SSA1                                   
316000     MOVE 'WLBENA12 ' TO SSA2                                             
316100     MOVE '  ' TO GODK-STATUSKODER                                        
316200     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
316300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
316400     PERFORM IMS-STATUSKONTROLL                                           
316500     .                                                                    
316600     EJECT                                                                
316700 IMS-GU-XXAI01 SECTION.                                                   
316800     STRING 'WLXXAI01(WDGXKEY  =' W-1207-KEY-X ')'                        
316900            DELIMITED BY SIZE INTO SSA1                                   
317000     MOVE '  ' TO GODK-STATUSKODER                                        
317100     CALL CBLTDLI USING GU XXAI-PCB DLI-IO-AREA-XXAI SSA1                 
317200     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
317300     PERFORM IMS-STATUSKONTROLL                                           
317400     .                                                                    
317500     SKIP2                                                                
317600 IMS-GHNP-XXAI11 SECTION.                                                 
317700     MOVE 'WLXXAI11 ' TO SSA1                                             
317800     MOVE '  ' TO GODK-STATUSKODER                                        
317900     CALL CBLTDLI USING GHNP XXAI-PCB DLI-IO-AREA-XXAI SSA1               
318000     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
318100     PERFORM IMS-STATUSKONTROLL                                           
318200     .                                                                    
318300     SKIP2                                                                
318400 IMS-REPL-XXAI11 SECTION.                                                 
318500     MOVE '  ' TO GODK-STATUSKODER                                        
318600     CALL CBLTDLI USING REPL XXAI-PCB DLI-IO-AREA-XXAI                    
318700     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
318800     PERFORM IMS-STATUSKONTROLL                                           
318900     .                                                                    
319000     EJECT                                                                
319100 IMS-GET-SATB01 SECTION.                                                  
319200     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
319300            DELIMITED BY SIZE INTO SSA1                                   
319400     MOVE '  GE' TO GODK-STATUSKODER                                      
319500     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA-3 SSA1                   
319600     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
319700     PERFORM IMS-STATUSKONTROLL                                           
319800     .                                                                    
319900     SKIP2                                                                
320000 IMS-GHU-SATB11 SECTION.                                                  
320100     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
320200            DELIMITED BY SIZE INTO SSA1                                   
320300     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
320400            DELIMITED BY SIZE INTO SSA2                                   
320500     MOVE '  GE' TO GODK-STATUSKODER                                      
320600     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA-3 SSA1 SSA2              
320700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
320800     PERFORM IMS-STATUSKONTROLL                                           
320900     .                                                                    
321000     SKIP2                                                                
321100 IMS-GET-SATB11-CSEQ SECTION.                                             
321200     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
321300            DELIMITED BY SIZE INTO SSA1                                   
321400     MOVE 'WLSATB01 ' TO SSA2                                             
321500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
321600     CALL CBLTDLI USING GHN SATE-PCB DLI-IO-AREA-3 SSA1 SSA2              
321700     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
321800     PERFORM IMS-STATUSKONTROLL                                           
321900     .                                                                    
322000     EJECT                                                                
322100 IMS-GET-SATB11 SECTION.                                                  
322200     MOVE 'WLSATB11 ' TO SSA1                                             
322300     MOVE '  GE' TO GODK-STATUSKODER                                      
322400     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-3 SSA1                  
322500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
322600     PERFORM IMS-STATUSKONTROLL                                           
322700     .                                                                    
322800     SKIP2                                                                
322900 IMS-REPL-SATB SECTION.                                                   
323000     MOVE '  ' TO GODK-STATUSKODER                                        
323100     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA-3                       
323200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
323300     PERFORM IMS-STATUSKONTROLL                                           
323400     .                                                                    
323500     SKIP2                                                                
323600 IMS-ISRT-SATB01 SECTION.                                                 
323700     MOVE 'WLSATB01 ' TO SSA1                                             
323800     MOVE '  ' TO GODK-STATUSKODER                                        
323900     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA-3 SSA1                  
324000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
324100     PERFORM IMS-STATUSKONTROLL                                           
324200     .                                                                    
324300     SKIP3                                                                
324400 IMS-ISRT-XXID SECTION.                                                   
324500     STRING 'WLXXID01(WDG3KEY  =' W-9101-KEY-X ')'                        
324600              DELIMITED BY SIZE INTO SSA1                                 
324700     MOVE 'WLXXID11*L' TO SSA2                                            
324800     MOVE '  ' TO GODK-STATUSKODER                                        
324900     CALL CBLTDLI USING ISRT XXID-PCB DLI-IO-AREA SSA1 SSA2               
325000     MOVE XXID-STATUS-CODE TO STATUS-WS                                   
325100     PERFORM IMS-STATUSKONTROLL                                           
325200     .                                                                    
325300     EJECT                                                                
325400 IMS-GU-XXAQ01 SECTION.                                                   
325500     STRING 'WLXXAQ01(WDGXKEY  =' W-1131-KEY-X ')'                        
325600            DELIMITED BY SIZE INTO SSA1                                   
325700     MOVE '  GE' TO GODK-STATUSKODER                                      
325800     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA SSA1                      
325900     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
326000     PERFORM IMS-STATUSKONTROLL                                           
326100     .                                                                    
326200     SKIP3                                                                
326300 IMS-GNP-XXAQ11 SECTION.                                                  
326400     MOVE 'WLXXAQ11 ' TO SSA1                                             
326500     MOVE '  GE' TO GODK-STATUSKODER                                      
326600     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA SSA1                     
326700     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
326800     PERFORM IMS-STATUSKONTROLL                                           
326900     .                                                                    
327000     EJECT                                                                
327100 IMS-ISRT-WDGX2264-NEW SECTION.                                           
327200                                                                          
327300     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
327400          DELIMITED BY SIZE INTO SSA1                                     
327500     MOVE 'WDGX2264 '      TO SSA2                                        
327600     MOVE '  II'           TO GODK-STATUSKODER                            
327700     CALL CBLTDLI USING ISRT WDR2-N-PCB DLI-IO-WDGX2264-N SSA1            
327800                                                          SSA2            
327900     MOVE WDR2-N-STATUS-CODE TO STATUS-WS                                 
328000     PERFORM IMS-STATUSKONTROLL                                           
328100     .                                                                    
328200                                                                          
328300 IMS-ISRT-WDGX2266-NEW SECTION.                                           
328400                                                                          
328500     MOVE 'WDGX2266 '      TO SSA1                                        
328600     MOVE '  '             TO GODK-STATUSKODER                            
328700     CALL CBLTDLI USING ISRT WDR2-N-PCB DLI-IO-WDGX2266-N SSA1            
328800     MOVE WDR2-N-STATUS-CODE TO STATUS-WS                                 
328900     PERFORM IMS-STATUSKONTROLL                                           
329000     .                                                                    
329100     EJECT                                                                
329200 IMS-GHU-WDGX2264-OLD SECTION.                                            
329300                                                                          
329400     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
329500          DELIMITED BY SIZE INTO SSA1                                     
329600     STRING 'WDGX2264(TISOP    =' W-TISOP-O-X ')'                         
329700          DELIMITED BY SIZE INTO SSA2                                     
329800     MOVE '  GE'              TO GODK-STATUSKODER                         
329900     CALL CBLTDLI USING GHU WDR2-O-PCB DLI-IO-WDGX2264-O SSA1 SSA2        
330000     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
330100     PERFORM IMS-STATUSKONTROLL                                           
330200     .                                                                    
330300     EJECT                                                                
330400 IMS-GHNP-WDGX2266-OLD SECTION.                                           
330500                                                                          
330600     STRING 'WDGX2266(KY2266  >=' W-W2266KY-MIN-O-X                       
330700                    '&KY2266  <=' W-W2266KY-MAX-O-X ')'                   
330800          DELIMITED BY SIZE INTO SSA1                                     
330900     MOVE '  GE'              TO GODK-STATUSKODER                         
331000     CALL CBLTDLI USING GHNP WDR2-O-PCB DLI-IO-WDGX2266-O SSA1            
331100     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
331200     PERFORM IMS-STATUSKONTROLL                                           
331300     .                                                                    
331400 IMS-GNP-WDGX2266     SECTION.                                            
331500                                                                          
331600     MOVE 'WDGX2266 '           TO  SSA1                                  
331700     MOVE '  GE'              TO GODK-STATUSKODER                         
331800     CALL CBLTDLI USING GNP WDR2-O-PCB DLI-IO-WDGX2266-O SSA1             
331900     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
332000     PERFORM IMS-STATUSKONTROLL                                           
332100     .                                                                    
332200     EJECT                                                                
332300 IMS-DLET-WDGX2266-OLD SECTION.                                           
332400                                                                          
332500     MOVE '  '       TO GODK-STATUSKODER                                  
332600     CALL CBLTDLI USING DLET WDR2-O-PCB DLI-IO-WDGX2266-O                 
332700     MOVE WDR2-O-STATUS-CODE TO STATUS-WS                                 
332800     PERFORM IMS-STATUSKONTROLL                                           
332900     .                                                                    
333000     EJECT                                                                
333100 IMS-DLET-WDGX2264-OLD SECTION.                                           
333200                                                                          
333300     MOVE '  '       TO GODK-STATUSKODER                                  
333400     CALL CBLTDLI USING DLET WDR2-O-PCB DLI-IO-WDGX2264-O                 
333500     MOVE WDR2-O-STATUS-CODE TO STATUS-WS                                 
333600     PERFORM IMS-STATUSKONTROLL                                           
333700     .                                                                    
333800     EJECT                                                                
333900 IMS-STATUSKONTROLL SECTION.                                              
334000     SET STATUS-IX TO 1                                                   
334100     SEARCH GODK-STATUS AT END CALL FELLOG                                
334200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
334300     END-SEARCH                                                           
334400     .                                                                    
