000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6030800.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   97/01/07.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    OMSTRUKTURERAT 2007/07 AV J.NIHLBLAD                                 
000900*                                                                         
001000*                                                                         
001100*    FUNKTION:                                                            
001200*      - VISNING OCH UPPDATERING AV SPÄRRKOD OCH SPÄRRNOT.                
001300*        EN ARTIKEL KAN SPÄRRAS ANTINGEN GLOBALT ELLER LOKALT.            
001400*        OCH GÖRS PÅ GRUND AV KVALITéTS-PROBLEM.                          
001500*        ARTIKEL STOPPAD FÖR UTLEVERANS                                   
001600*        00 = INGEN SPÄRR'                                                
001700*        20 = GLOBALSPÄRR PÅ CDC/SDC/NDC   UPPDAT AV CDC.                 
001800*        21 = LEVSPÄRR    PÅ CDC/SDC/NDC.  UPPDAT AV CDC.                 
001900*        22 = LEVSPÄRR    PÅ     SDC/NDC.  UPPDAT AV SDC/NDC.             
002000*                                                                         
002100*    GLOBALT:                                                             
002200*        GLOBAL SPÄRRKOD = 00 ELLER 20.                                   
002300*      - LOKALA "SITER" = CDC, SDC21,22,23,24,25, NDC41,42,43,51.         
002400*      - GLOBAL KOD 20 SKALL SLÅS PÅ TILL VARJE DC,                       
002500*        OCH FÅR BARA SÄTTAS AV CDC-ANVÄNDARE.                            
002600*        KOD 20 SLÅR ÖVER EN TIDIGARE SATT 21 och 22.                     
002700*      - GLOBAL KOD 00 PROPAGERAS NED TILL VARJE DC,                      
002800*        SOM INTE FÅTT NY KOD OCH KVANT I SAMMA INMATNING.                
002900*                                                                         
003000*    LOKALT CDC11                                                         
003100*      - LOKAL SPÄRRKOD = 00 ELLER 21.                                    
003200*      - SPÄRRKOD FÅR BARA UPPDATERAS OM DET INNAN INTE VAR 20.           
003300*        I SÅ FALL MÅSTE GLOBAL INNAN/SAMTIDIGT UPPDATERAS MED 00.        
003400*      - KVANT KAN BARA UPPDATERAS VID KDLEVSP=00.                        
003500*                                                                         
003600*    LOKALT ALLA SDC + NDC                                                
003700*      - LOKAL SPÄRRKOD = 00 ELLER 21 ELLER 22.                           
003800*      - SPÄRRKOD FÅR BARA UPPDATERAS OM DET INNAN INTE VAR 20.           
003900*        I SÅ FALL MÅSTE GLOBAL INNAN/SAMTIDIGT UPPDATERAS MED 00.        
004000*        (DETTA FÅR BARA GÖRAS AV CDC-FOLK)                               
004100*                                                                         
004200*      - KOD 21 FÅR BARA UPPDATERAS/NOLLAS  AV CDC-FOLK.                  
004300*        OM KOD 21 SÄTTS PÅ CDC, PROPAGERAS KODEN TILL ÖVRIGA DC          
004400*        MEN BARA OM KVSPARR-KVAL DÄR ÄR NOLL.                            
004500*      - KVANT KAN BARA UPPDATERAS VID KDLEVSP=00.                        
004600*                                                                         
004700*      - KOD 22 FÅR UPPDATERAS AV SDC- OCH NDC-FOLK PÅ EGET DC,           
004800*        SAMT AV CDC-PERSONAL PÅ ALLA SDC OCH NDC.                        
004900*        MEN BARA VID KDLEVSP=00 PÅ ARTC11 OCH EGET ARTS11.               
005000*                                                                         
005100*        PROGRAMMET LÄSER+UPPDATERAR   WLARTC (WDK6)                      
005200*        PROGRAMMET LÄSER+UPPDATERAR   WLARTS (WDK7)                      
005300*        PROGRAMMET LÄSER+UPPDATERAR   WLOIGA (WDL7)                      
005400*        PROGRAMMET LÄSER              WLBENA (WDD3)                      
005500*        PROGRAMMET LÄSER              W6D2                               
005600*        PROGRAMMET LÄSER              WDP3                               
005700*        PROGRAMMET LÄSER              WDP3A                              
005800*        PROGRAMMET LÄSER              WDP3b                              
005900*        PROGRAMMET LÄSER              WDP3c                              
006000*                                                                         
006100*    INDATA.                                                              
006200*        TRANSAKTION: W6T308                                              
006300*        MID:         W6I30801                                            
006400*                                                                         
006500*    UTDATA.                                                              
006600*        MOD:         W6O30801                                            
006700                                                                          
006800     SKIP3                                                                
006900 ENVIRONMENT DIVISION.                                                    
007000     EJECT                                                                
007100 DATA DIVISION.                                                           
007200 WORKING-STORAGE SECTION.                                                 
007300                                                                          
007400*    -- CHECKED BY WY2000                                                 
007500 77  IDPGM                       PIC X(08)   VALUE 'W6030800'.            
007600                                                                          
007700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007900                                                                          
008000 77  JA                          PIC X       VALUE 'J'.                   
008100 77  NEJ                         PIC X       VALUE 'N'.                   
008200 77  SW-MAIL                     PIC X       VALUE 'N'.                   
008300 77  SW-TEXT-FINNS               PIC X       VALUE 'N'.                   
008400 77  SW-UPD-SDC-20-21            PIC X       VALUE 'N'.                   
008500 77  SPAR-IDANSK                 PIC S9(3)   VALUE ZERO.                  
008600 77  SPAR-IDBERED                PIC S9(3)   VALUE ZERO.                  
008700 77  SPAR-IDINK                  PIC S9(3)   VALUE ZERO.                  
008800 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
008900 77  SPAR-IDFKNGRP               PIC 9(5)    VALUE ZERO.                  
009000 77  SPAR-IDPERSON               PIC 9(3)    VALUE ZERO.                  
009100 77  SPAR-IDMAIL1                PIC X(60)   VALUE SPACE.                 
009200 77  SPAR-IDMAIL2                PIC X(60)   VALUE SPACE.                 
009300 77  SPAR-IDMAIL3                PIC X(60)   VALUE SPACE.                 
009400 77  SPAR-IDMAIL4                PIC X(60)   VALUE SPACE.                 
009500 77  SW-TRAEFF                   PIC X       VALUE SPACE.                 
009600 77  WS-TEARTNOT-GLOB            PIC X(40)   VALUE SPACE.                 
009700 77  WS-DAGENS-TID               PIC S9(16)  COMP-3.                      
009800 77  WS-DAGENS-DATUM             PIC S9(16)  COMP-3.                      
009900                                                                          
010000*    --- INDEXFÄLT                                                        
010100 77  DCIX                        PIC S9(9)   VALUE +0 COMP-3.             
010200 77  DCMAX                       PIC S9(9)   VALUE +13 COMP-3.            
010300 77  IX                          PIC S9(9)   VALUE +0 COMP-3.             
010400 77  IX-MAX                      PIC S9(9)   VALUE +7 COMP-3.             
010500 77  RAD-IX                      PIC S9(9)   VALUE +0 COMP-3.             
010600 77  RAD-IX-MAX                  PIC S9(9)   VALUE +12 COMP-3.            
010700                                                                          
010800*    --- ARBETSFÄLT ÖVRIGA                                                
010900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011000                                                                          
011100 77  DISPONIBELT                 PIC S9(7)   COMP-3 VALUE +0.             
011200 77  W-ART-KVLS                  PIC S9(7)   COMP-3 VALUE +0.             
011300 77  W-ART-KVUTRS                PIC S9(7)   COMP-3 VALUE +0.             
011400 77  W-ART-KVRESS                PIC S9(7)   COMP-3 VALUE +0.             
011500 77  W-ART-KVSPANT               PIC S9(7)   COMP-3 VALUE +0.             
011600 77  W-ART-KDERS                 PIC S9(3)   COMP-3 VALUE +0.             
011700 77  W-ART-KVSLAGER              PIC S9(7)   COMP-3 VALUE +0.             
011803 77  WS-NEW-KVSPARR-KVAL         PIC S9(7)   COMP-3 VALUE +0.             
011903 77  W-ART-KDLTK                 PIC S9      COMP-3 VALUE +0.             
012003 77  SPAR-CLAG-KDLEVSP           PIC 9(2)    VALUE ZERO.                  
012103 77  WS-NEW-KDLEVSP              PIC 9(2)    VALUE ZERO.                  
012203 77  WS-IDDC-TREE                PIC X(2)    VALUE SPACE.                 
012303 77  WS-NEW-IDDC                 PIC X(2)    VALUE SPACE.                 
012403 77  WS-LEVEL                    PIC 9       VALUE ZERO.                  
012503 77  WS-SHOW                     PIC X       VALUE SPACE.                 
012603 77  WS-TYPE                     PIC X       VALUE SPACE.                 
012703 77  WS-OPPNA                    PIC X       VALUE 'N'.                   
012803 77  W-IDUSER1                   PIC X(8)    VALUE SPACE.                 
012903 77  W-IDUSER2                   PIC X(8)    VALUE SPACE.                 
013003 77  W-IDUSER3                   PIC X(8)    VALUE SPACE.                 
013103 77  W-IDUSER4                   PIC X(8)    VALUE SPACE.                 
013203                                                                          
013303                                                                          
013403*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
013503                                                                          
013603 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013703     88  NYCKLAR-OK                          VALUE 'J'.                   
013803     88  NYCKLAR-FEL                         VALUE 'N'.                   
013903                                                                          
014003 77  INDATA-FINNS-SW             PIC X       VALUE 'N'.                   
014103     88  INDATA-FINNS                        VALUE 'J'.                   
014203     88  INDATA-SAKNAS                       VALUE 'N'.                   
014303                                                                          
014403 77  INDATA-SW                   PIC X       VALUE 'N'.                   
014503     88  INDATA-OK                           VALUE 'J'.                   
014603     88  INDATA-FEL                          VALUE 'N'.                   
014703                                                                          
014803 77  STOCK-BALANCE-SW            PIC X       VALUE 'N'.                   
014903     88  STOCK-BAL-OK                        VALUE 'J'.                   
015003     88  STOCK-BAL-NO                        VALUE 'N'.                   
015103                                                                          
015203 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015303     88  EGEN-MID                            VALUE '6308'.                
015403     88  GODK-MID                            VALUE '6304'                 
015503                                                   '6305'                 
015603                                                   '6308'.                
015703     88  HELP-MID                            VALUE '0551'.                
015803                                                                          
015804 01  NYCKLAR-TILL-DLI.                                                    
015805                                                                          
015911                                                                          
015912     03  W-2403KEY-X.                                                     
015913         05  W-2403-IDHTYP      PIC X(4)     VALUE '2403'.                
015914         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
015915     03  W-2404KEY-MIN-X.                                                 
015916         05  W-IDARTNR-MIN      PIC S9(9)    VALUE ZERO COMP-3.           
015917         05  W-IDDC-KY-MIN      PIC X(2)     VALUE LOW-VALUE.             
015918         05  W-IDFKNGRP-KY-MIN  PIC S9(5)    VALUE ZERO COMP-3.           
015919     03  W-2404KEY-MAX-X.                                                 
015920         05  W-IDARTNR-MAX      PIC S9(9)  VALUE 999999999 COMP-3.        
015921         05  W-IDDC-KY-MAX      PIC X(2)     VALUE HIGH-VALUE.            
015922         05  W-IDFKNGRP-KY-MAX  PIC S9(5)    VALUE +99999 COMP-3.         
015923                                                                          
015930*      --- VALID IDDC CODES                                               
016003*                                                                         
016103*01    -COPY WWDC99                                                       
016203*01    -COPY WWDC99 -PRE PERS-                                            
016303*01    -COPY WWDC99 -PRE BLK-                                             
016403*01    -COPY WWDCKONS                                                     
016503       EJECT                                                              
016603*    --- OBS: Vid förändring av antal DC, komihåg att ändra DCMAX         
016703                                                                          
016803*    KONTROLLAREA FÖR INMATAT IDSKYLT-VÄRDE (SPRÅKKOD)                    
016903*    INNEHÅLLER FÄLTEN 02-IDSKYLT OCH 02-GODK-IDSKYLT                     
017003                                                                          
017103*01  -COPY WWLAND02  -PRE WS-                                             
017203                                                                          
017303     EJECT                                                                
017403*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017503 01  GENERELLA-SUBPROGRAM.                                                
017603     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017703     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017803     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017903     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018003     EJECT                                                                
018103*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018203*01 -COPY WMEDAREA                                                        
018303     SKIP3                                                                
018403 01  MESSAGE-CODES.                                                       
018503     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018603     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
018703     03  ERR-MISSING-IN-REGISTER PIC X(3)    VALUE '010'.                 
018803     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018903     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019003     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019103     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
019203     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
019303     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
019403     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
019503     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019603     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
019703     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
019803     SKIP3                                                                
019903 01  SPAR-AREA.                                                           
020003     03  SPAR-IDTRANS                PIC X(4)  VALUE '6308'.              
020103     03  SPAR-IDARTNR-ENTER          PIC S9(9) VALUE ZERO COMP-3.         
020203     03  SPAR-IDSKYLT-ENTER          PIC X(2)  VALUE SPACE.               
020303     03  SPAR-IDDC-ENTER             PIC X(2)  VALUE SPACE.               
020403     03  SPAR-IDDC-LEVEL1            PIC X(2)  VALUE SPACE.               
020503     03  SPAR-IDDC-LEVEL2            PIC X(2)  VALUE SPACE.               
020603     03  SPAR-IDDC-LEVEL3            PIC X(2)  VALUE SPACE.               
020703     03  SPAR-IDDC2                  PIC X(2)  VALUE SPACE.               
020803     03  SPAR-IDDC-LEVEL1-ENTER      PIC X(2)  VALUE SPACE.               
020903     03  SPAR-IDDC-LEVEL2-ENTER      PIC X(2)  VALUE SPACE.               
021003     03  SPAR-IDDC-LEVEL3-ENTER      PIC X(2)  VALUE SPACE.               
021103     03  SPAR-IDDC2-ENTER            PIC X(2)  VALUE SPACE.               
021203     03  SPAR-SHOW-ENTER             PIC X     VALUE SPACE.               
021303     03  SPAR-TYPE-ENTER             PIC X     VALUE SPACE.               
021403     03  SPAR-IDDC-NEXT              PIC X(2)  VALUE SPACE.               
021503     03  SPAR-TABELL-IDDC            OCCURS 12.                           
021603         05 SPAR-IDDC-TAB            PIC X(2)  VALUE SPACE.               
021703     03  SPAR-TABELL-CLOSE           OCCURS 12.                           
021803         05 SPAR-CLOSE-TAB           PIC X     VALUE SPACE.               
021903     03  FILLER                      PIC X(1000) VALUE SPACE.             
022003     EJECT                                                                
022103 01  MESSAGE-TEXT1.                                                       
022203     03  INF-MAIL-SENT                                                    
022303                                 PIC X(20)   VALUE                        
022403         'MAIL SENT           '.                                          
022503     EJECT                                                                
022603 01  PROG-TO-PROG-SW.                                                     
022703*    03  -COPY WMSGSOP                                                    
022803     EJECT                                                                
022903 01  WS-PARAMETRAR.                                                       
023003     03  WS-URVAL.                                                        
023103         05 URV-IDARTNR     PIC 9(9).                                     
023203         05 URV-IDLEVNR     PIC X(5).                                     
023303         05 URV-IDPERSON    PIC 9(3).                                     
023403     03  WS-IDMAIL1.                                                      
023503         05 URV-IDMAIL1     PIC X(60).                                    
023603     03  WS-IDMAIL2.                                                      
023703         05 URV-IDMAIL2     PIC X(60).                                    
023803     03  WS-IDMAIL3.                                                      
023903         05 URV-IDMAIL3     PIC X(60).                                    
024003     03  WS-IDMAIL4.                                                      
024103         05 URV-IDMAIL4     PIC X(60).                                    
024203     EJECT                                                                
024303 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
024403     SKIP3                                                                
024503*01 -COPY WMSGINIT                                                        
024603     EJECT                                                                
024703*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024803*                                                                         
024903 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
025003     SKIP3                                                                
025103*01  MID -COPY W6I30801                                                   
025203     EJECT                                                                
025303 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025403     SKIP3                                                                
025503*01  -COPY WMSGAREA                                                       
025603     EJECT                                                                
025703     03  MOD REDEFINES MSG-AREA.                                          
025803*      05  -COPY W6O30801                                                 
025903     EJECT                                                                
026003 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026103     SKIP3                                                                
026203*01  -COPY WMFSAREA                                                       
026303     EJECT                                                                
026403*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026503 01  FILLER                      PIC X(16)  VALUE 'TEST-WLARTC11'.        
026603 01  TEST-WLARTC11-AREA.                                                  
026703*    03 -COPY WDK611  -PRE TEST-                                          
026803     EJECT                                                                
026903 01  FILLER                      PIC X(16)  VALUE 'TEST-WLARTS11'.        
027003 01  TEST-WLARTS11-AREA.                                                  
027103*    03 -COPY WDK711  -PRE TEST-                                          
027203*                                                                         
027303     EJECT                                                                
027403 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027503     SKIP3                                                                
027603 01  NYCKLAR-TILL-DLI.                                                    
027703     03  W-IDARTNR-X.                                                     
027803         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027903     03  W-KDSEGKEY-X.                                                    
028003         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
028103     03  W-IDDC-REF-X.                                                    
028203         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
028303     03  W-KDNOTTYP-X.                                                    
028403         05  W-KDNOTTYP          PIC S9      VALUE +4 COMP-3.             
028503     03  W-IDDC-X.                                                        
028603         05  W-IDDC              PIC XX      VALUE SPACE.                 
028703     03  W-IDDC-K7-X.                                                     
028803         05  W-IDDC-K7           PIC XX      VALUE SPACE.                 
028903     03  W-IDDC-X2.                                                       
029003         05  W-IDDC2             PIC XX      VALUE SPACE.                 
029103     03  W-IDSKYLT-X.                                                     
029203         05  W-IDSKYLT           PIC XXX     VALUE SPACE.                 
029303     03  W-IDPERSON-X.                                                    
029403         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
029503     03  W-KDARBTYP-X.                                                    
029603         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
029703     03  W-WDP3A1-MIN.                                                    
029803         05  W-IDLANDA1-MIN        PIC X(2)    VALUE space.               
029903         05  W-IDARTNRF-MIN        PIC S9(9)   VALUE ZERO COMP-3.         
030003         05  W-IDARTNRT-MIN        PIC S9(9)   VALUE ZERO COMP-3.         
030103         05  W-KDARBTYP-A-MIN      PIC X(8)    VALUE SPACE.               
030203     03  W-WDP3A1-MAX.                                                    
030303         05  W-IDLANDA1-MAX        PIC X(2)    VALUE space.               
030403         05  W-IDARTNRF-MAX        PIC S9(9)   VALUE ZERO COMP-3.         
030503         05  W-IDARTNRT-MAX        PIC S9(9)   VALUE ZERO COMP-3.         
030603         05  W-KDARBTYP-A-MAX      PIC X(8)    VALUE SPACE.               
030703     03  W-WDP3B1-X.                                                      
030803         05  W-IDLAND-B            PIC X(2)    VALUE space.               
030903         05  W-IDLEVNR-B           PIC X(5)   VALUE SPACE.                
031003         05  W-KDARBTYP-B          PIC X(8)   VALUE SPACE.                
031103     03  W-WDP3C1-MIN.                                                    
031203         05  W-IDLANDC1-MIN        PIC X(2)    VALUE space.               
031303         05  W-IDFKNGRPF-MIN       PIC S9(5)   VALUE ZERO COMP-3.         
031403         05  W-IDFKNGRPT-MIN       PIC S9(5)   VALUE ZERO COMP-3.         
031503         05  W-KDARBTYP-C-MIN      PIC X(8)    VALUE SPACE.               
031603     03  W-WDP3C1-MAX.                                                    
031703         05  W-IDLANDC1-MAX        PIC X(2)    VALUE space.               
031803         05  W-IDFKNGRPF-MAX       PIC S9(5)   VALUE ZERO COMP-3.         
031903         05  W-IDFKNGRPT-MAX       PIC S9(5)   VALUE ZERO COMP-3.         
032003         05  W-KDARBTYP-C-MAX      PIC X(8)    VALUE SPACE.               
032103     03  W-WDGXKEY-6331.                                                  
032203         05  W-6331-IDHTYP       PIC X(4)    VALUE '6331'.                
032303         05  W-6331-IDDC         PIC X(2)    VALUE '11'.                  
032403         05  W-6331-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
032503     03  W-IDDC-6332.                                                     
032603         05  W-6332-IDDC         PIC X(2)    VALUE SPACE.                 
032703     03  W-IDDC-6334.                                                     
032803         05  W-6334-IDDC         PIC X(2)    VALUE SPACE.                 
032903     03  W-IDDC-B6-X.                                                     
033003         05  W-IDDC-B6          PIC X(2)       VALUE SPACE.               
033103     SKIP3                                                                
033203*    ARTIKLAR KLARA FÖR TÄCKNING                                          
033303*    RESTORDERREGISTER (PASSIVT)                                          
033403*    FYSISK NYCKEL: WDGXKEY                                               
033503*    (IDHTYP + IDDC + LOWVALUE)                                           
033603*    NAME=WL450511,PARENT=WL450501,SOURCE=((WDGX4506,,WDR4))              
033703                                                                          
033803     03  W-WDGX-4505-KEY-X.                                               
033903         05 W-IDHTYP-4505        PIC X(04)  VALUE '4505'.                 
034003         05 W-IDDC-4505          PIC X(02)  VALUE SPACE.                  
034103         05 FILLER               PIC X(24)  VALUE LOW-VALUE.              
034203     SKIP2                                                                
034303*    --- STATUS-KOD FRÅN IMS                                              
034403 01  STATUS-WS                   PIC XX.                                  
034503     88  SEGMENT-FINNS                       VALUE '  '.                  
034603     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
034703     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
034803     88  SEGMENT-SLUT                        VALUE 'GB'.                  
034903     SKIP2                                                                
035003 01  GODK-STATUSKODER.                                                    
035103     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035203     SKIP3                                                                
035303 01  SSA1                        PIC X(128).                              
035403 01  SSA2                        PIC X(64).                               
035503 01  SSA3                        PIC X(64).                               
035603     EJECT                                                                
035703*    --- IMS FUNKTIONSKODER                                               
035803*01  -COPY W0003                                                          
035903     EJECT                                                                
036003*    ---  DLI INPUT-OUTPUT AREA                                           
036103                                                                          
036203 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
036303 01  DLI-IO-WLARTC01.                                                     
036403*    03  -COPY WDK601  -PRE ARTC-                                         
036503     EJECT                                                                
036603                                                                          
036703 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
036803 01  DLI-IO-WLARTC11.                                                     
036903*    03  -COPY WDK611  -PRE ARTC-                                         
037003     EJECT                                                                
037103                                                                          
037203 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC25'.                    
037303 01  DLI-IO-WLARTC25.                                                     
037403*    03  -COPY WDK625  -PRE ARTC-                                         
037503     EJECT                                                                
037603                                                                          
037604 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611  '.                    
037605 01  DLI-IO-WDK611.                                                       
037606*    03  -COPY WDK611                                                     
037607                                                                          
037703 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
037803 01  DLI-IO-WDK629.                                                       
037903*    03  -COPY WDK629  -PRE WDK6-                                         
038003     EJECT                                                                
038103                                                                          
038203 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS01'.                    
038303 01  DLI-IO-WLARTS01.                                                     
038403*    03  -COPY WDK701  -PRE ARTS-                                         
038503     EJECT                                                                
038603                                                                          
038703 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
038803 01  DLI-IO-WLARTS11.                                                     
038903*    03  -COPY WDK711  -PRE ARTS-                                         
039003     EJECT                                                                
039103                                                                          
039203 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701  '.                    
039303 01  DLI-IO-WDK701.                                                       
039403*    03  -COPY WDK701  -PRE WDK7-                                         
039503     EJECT                                                                
039603                                                                          
039703 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711  '.                    
039803 01  DLI-IO-WDK711.                                                       
039903*    03  -COPY WDK711  -PRE WDK7-                                         
040003     EJECT                                                                
040103                                                                          
040203 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA01'.                    
040303 01  DLI-IO-WLBENA01.                                                     
040403*    03  -COPY WDD301  -PRE BENA-                                         
040503     EJECT                                                                
040603                                                                          
040703 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA11'.                    
040803 01  DLI-IO-WLBENA11.                                                     
040903*    03  -COPY WDD311  -PRE BENA-                                         
041003     EJECT                                                                
041103                                                                          
041203 01  FILLER         PIC X(24) VALUE 'DLI-IO-WL450511'.                    
041303 01  DLI-IO-WL450511.                                                     
041403*    03  WL450511 -COPY WDGX4506                                          
041503     EJECT                                                                
041603 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6D201'.                      
041703 01  DLI-IO-W6D201.                                                       
041803*    03  -COPY W6D201                                                     
041903     EJECT                                                                
042003 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6D211'.                      
042103 01  DLI-IO-W6D211.                                                       
042203*    03  -COPY W6D211                                                     
042303     EJECT                                                                
042403 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDP311'.                      
042503 01  DLI-IO-WDP311.                                                       
042603*    03  -COPY WDP311                                                     
042703     EJECT                                                                
042803 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDP3A'.                       
042903 01  DLI-IO-WDP3A.                                                        
043003*    03  -COPY WDP3A1.                                                    
043103     EJECT                                                                
043203 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDP3B'.                      
043303 01  DLI-IO-AREA-WDP3B.                                                   
043403*    03  -COPY WDP3B1                                                     
043503     EJECT                                                                
043603 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDP3C'.                      
043703 01  DLI-IO-AREA-WDP3C.                                                   
043803*    03  -COPY WDP3C1                                                     
043903     EJECT                                                                
044003****WDR2                                                                  
044103 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6331'.                    
044203 01  DLI-IO-WDGX6331.                                                     
044303*    03  -COPY WDGX6331                                                   
044403     EJECT                                                                
044503 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6332'.                    
044603 01  DLI-IO-WDGX6332.                                                     
044703*    03  -COPY WDGX6332                                                   
044803     EJECT                                                                
044903 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6334'.                    
045003 01  DLI-IO-WDGX6334.                                                     
045103*    03  -COPY WDGX6334                                                   
045203     EJECT                                                                
045303 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
045403 01   DLI-IO-AREA-B601.                                                   
045503*     03  -COPY WDB601                                                    
045603     EJECT                                                                
045604 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2404'.                    
045605 01  DLI-IO-WDGX2404.                                                     
045606*    03  -COPY WDGX2404                                                   
045607                                                                          
045703 LINKAGE SECTION.                                                         
045803*01  -COPY W0009   -PRE MSG-                                              
045903*01  -COPY W0009   -PRE ALT-                                              
046003     EJECT                                                                
046103*01  -COPY W0008   -PRE USEA-                                             
046203     05  FILLER                  PIC X.                                   
046303     EJECT                                                                
046403*01  -COPY W0008  -PRE 4505-                                              
046503     05  FILLER                  PIC X.                                   
046603     EJECT                                                                
046703*01  -COPY W0008  -PRE ARTC-                                              
046803     05  FILLER                  PIC X.                                   
046903     EJECT                                                                
047003*01  -COPY W0008  -PRE WDK6-                                              
047103     05  FILLER                  PIC X.                                   
047203     EJECT                                                                
047303*01  -COPY W0008  -PRE ARTS-                                              
047403     05  FILLER                  PIC X.                                   
047503     EJECT                                                                
047603*01  -COPY W0008  -PRE WDK7-                                              
047703     05  FILLER                  PIC X.                                   
047803     EJECT                                                                
047903*01  -COPY W0008  -PRE BENA-                                              
048003     05  FILLER                  PIC X.                                   
048103     EJECT                                                                
048203*01  -COPY W0008  -PRE W6D2-                                              
048303     05  FILLER                  PIC X.                                   
048403     EJECT                                                                
048503*01  -COPY W0008  -PRE WDP3-                                              
048603     05  FILLER                  PIC X.                                   
048703     EJECT                                                                
048803*01  -COPY W0008  -PRE WDP3A-                                             
048903     05  FILLER                  PIC X.                                   
049003     EJECT                                                                
049103*01  -COPY W0008  -PRE WDP3B-                                             
049203     05  FILLER                  PIC X.                                   
049303     EJECT                                                                
049403*01  -COPY W0008  -PRE WDP3C-                                             
049503     05  FILLER                  PIC X.                                   
049603     EJECT                                                                
049703*01  -COPY W0008  -PRE 6331-                                              
049803     05  FILLER                         PIC X(4).                         
049903     05  6331-KEY-FB-AREA-IDDC          PIC X(2).                         
050003     05  FILLER                         PIC X(24).                        
050103     05  6332-KEY-FB-AREA-IDDC          PIC X(2).                         
050203     EJECT                                                                
050303*01  -COPY W0008  -PRE 63312-                                             
050403     05  FILLER                  PIC X.                                   
050503     EJECT                                                                
050603*01  -COPY W0008  -PRE WDB6-                                              
050703     05  FILLER                  PIC X.                                   
050803     EJECT                                                                
050804*01  -COPY W0008  -PRE 2404-                                              
050805     05  FILLER                  PIC X.                                   
050806     EJECT                                                                
050903                                                                          
051003 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
051103                 WDK6-PCB ARTS-PCB WDK7-PCB BENA-PCB                      
051203                 4505-PCB WDP3-PCB                                        
051303                 WDP3A-PCB W6D2-PCB WDP3B-PCB WDP3C-PCB 6331-PCB          
051403                 WDB6-PCB 2404-PCB.                                       
051503 MAIN SECTION.                                                            
051603     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
051703                 WDK6-PCB ARTS-PCB WDK7-PCB BENA-PCB                      
051803                 4505-PCB WDP3-PCB                                        
051903                 WDP3A-PCB W6D2-PCB WDP3B-PCB WDP3C-PCB 6331-PCB          
052003                 WDB6-PCB 2404-PCB.                                       
052103                                                                          
052203     PERFORM IMS-GET-MSG                                                  
052303     IF SEGMENT-FINNS                                                     
052403       PERFORM A-INIT                                                     
052503       PERFORM B-KOLLA-NYCKLAR                                            
052603       IF NYCKLAR-OK                                                      
052703         IF MFS-UPDATE                                                    
052803           PERFORM G-KOLLA-INDATA                                         
052903           IF INDATA-OK                                                   
053003               PERFORM H-UPPDATERA                                        
053103           END-IF                                                         
053203         ELSE                                                             
053303           IF MFS-UPD-V                                                   
053403             PERFORM K-UPPDATERA                                          
053503           ELSE                                                           
053603             SET INDATA-OK TO TRUE                                        
053703             IF MFS-FIRST                                                 
053803               PERFORM C-FOERSTA-SIDA                                     
053903             ELSE                                                         
054003               IF MFS-NEXT                                                
054103                 PERFORM D-NAESTA-SIDA                                    
054203               ELSE                                                       
054303                 PERFORM E-BEHANDLA-ENTER                                 
054403               END-IF                                                     
054503             END-IF                                                       
054603           END-IF                                                         
054703         END-IF                                                           
054803         IF INDATA-OK                                                     
054903           PERFORM F-LAES-VISA-INFO                                       
055003         END-IF                                                           
055103       END-IF                                                             
055203                                                                          
055303       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30801 + 4                      
055403       PERFORM IMS-INSERT-MSG                                             
055503     END-IF                                                               
055603                                                                          
055703     MOVE ZERO TO RETURN-CODE                                             
055803     GOBACK                                                               
055903     .                                                                    
056003     EJECT                                                                
056103                                                                          
056203 A-INIT SECTION.                                                          
056303                                                                          
056403     IF MSG-DUBBLA-TRANSKODER                                             
056503       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30801                 
056603       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
056703       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
056803     ELSE                                                                 
056903       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I30801                  
057003       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
057103       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
057203     END-IF                                                               
057303                                                                          
057403     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
057503     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
057603     MOVE MFS-IDTRANS TO W-IDTRANS                                        
057703                                                                          
057803     MOVE LOW-VALUE TO MSG-AREA                                           
057903     MOVE 'W6O308N1' TO MFS-IDMOD                                         
058003     MOVE '6308' TO MOD-IDTRANS                                           
058103     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
058203                                                                          
058303     IF EGEN-MID OR HELP-MID                                              
058403       CONTINUE                                                           
058503     ELSE                                                                 
058603       MOVE SPACE TO MFS-KDTRTYP                                          
058703       MOVE '7' TO MFS-IDPFK                                              
058803     END-IF                                                               
058903                                                                          
059003     ACCEPT DAGENS-DATUM FROM DATE                                        
059103     MOVE LOW-VALUE  TO W-WDP3A1-MIN                                      
059203                        W-WDP3C1-MIN                                      
059303     MOVE HIGH-VALUE TO W-WDP3A1-MAX                                      
059403                        W-WDP3C1-MAX                                      
059503                                                                          
059603     MOVE 'SE'       TO W-IDLANDA1-MIN                                    
059703                        W-IDLANDA1-MAX                                    
059803                        W-IDLAND-B                                        
059903                        W-IDLANDC1-MAX                                    
060003                        W-IDLANDC1-MAX                                    
060103                                                                          
060203     ACCEPT WS-DAGENS-DATUM  FROM DATE                                    
060303     ACCEPT WS-DAGENS-TID    FROM TIME                                    
060403                                                                          
060503     .                                                                    
060603     EJECT                                                                
060703                                                                          
060803 B-KOLLA-NYCKLAR SECTION.                                                 
060903                                                                          
061003     MOVE ALL '+'            TO MSGI-WMSGINIT                             
061103     MOVE '001'              TO MSGI-KDCALL                               
061203     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
061303     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
061403     MOVE '6308'             TO MSGI-IDTRANS                              
061503     SET NYCKLAR-OK TO TRUE                                               
061603                                                                          
061703     IF EGEN-MID OR GODK-MID                                              
061803       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
061903     ELSE                                                                 
062003       IF MID-IDARTNR-IN NUMERIC                                          
062103         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
062203       END-IF                                                             
062303     END-IF                                                               
062403                                                                          
062503     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062603     MOVE SPACE          TO SPAR-IDSKYLT-ENTER                            
062703                            SPAR-IDDC-ENTER                               
062803                            SPAR-IDDC-LEVEL1                              
062903                            SPAR-IDDC-LEVEL2                              
063003                            SPAR-IDDC-LEVEL3                              
063103                            SPAR-IDDC2                                    
063203                            SPAR-SHOW-ENTER                               
063303                            SPAR-TYPE-ENTER                               
063403                            SPAR-IDDC-NEXT                                
063503                                                                          
063603     MOVE ZERO           TO SPAR-IDARTNR-ENTER                            
063703                                                                          
063803     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
063903                                                                          
064003     IF ENGLISH-TEXT                                                      
064103       MOVE 'N' TO MFS-KDHUVOMR                                           
064203       MOVE 'GB' TO WS-02-IDSKYLT   MED-IDSKYLT                           
064303     ELSE                                                                 
064403*      MOVE ZERO TO MFS-KDHUVOMR                                          
064503       MOVE 'N' TO MFS-KDHUVOMR                                           
064603       MOVE 'S ' TO WS-02-IDSKYLT   MED-IDSKYLT                           
064703     END-IF                                                               
064803                                                                          
064903*    -- KONTROLL AV VALT SPRÅK                                            
065003     MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-IN                               
065103                                                                          
065203     IF EGEN-MID OR HELP-MID                                              
065303       IF MID-IDSKYLT-IN = ALL '+' OR SPACE                               
065403         IF MID-IDSKYLT-UT NOT = SPACE                                    
065503           MOVE MID-IDSKYLT-UT TO WS-02-IDSKYLT                           
065603         END-IF                                                           
065703       ELSE                                                               
065803         MOVE MID-IDSKYLT-IN TO WS-02-IDSKYLT                             
065903         MOVE '7'   TO MFS-IDPFK                                          
066003         MOVE SPACE TO MFS-KDTRTYP                                        
066103       END-IF                                                             
066203     END-IF                                                               
066303                                                                          
066403     IF WS-02-GODK-IDSKYLT                                                
066503       MOVE WS-02-IDSKYLT TO W-IDSKYLT                                    
066603     ELSE                                                                 
066703       EVALUATE MSGI-IDLAND-SPR                                           
066803*        --- idland-spr är den internat.landsbeteckn. enl. iso-std        
066903*        --- idskylt  är den interna språkkoden för benämningsreg.        
067003         WHEN   'GB'                                                      
067103           MOVE 'GB ' TO W-IDSKYLT   MED-IDSKYLT                          
067203         WHEN   'SE'                                                      
067303           MOVE 'S  ' TO W-IDSKYLT   MED-IDSKYLT                          
067403         WHEN OTHER                                                       
067503           SET NYCKLAR-FEL TO TRUE                                        
067603       END-EVALUATE                                                       
067703     END-IF                                                               
067803                                                                          
067903*    -- KONTROLL AV IDARTNR                                               
068003                                                                          
068103     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
068203     IF MID-IDARTNR-IN NOT = ALL '+'                                      
068303       MOVE '7' TO MFS-IDPFK                                              
068403       MOVE SPACE TO MFS-KDTRTYP                                          
068503     ELSE                                                                 
068603       MOVE MFS-ROER-EJ-FAELT TO MOD-TIMAIL-KVAL                          
068703     END-IF                                                               
068803                                                                          
068903     INSPECT MSGI-IDARTNR REPLACING ALL SPACE BY ZERO                     
069003                                    ALL '+' BY ZERO                       
069103     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
069203       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
069303     ELSE                                                                 
069403       SET NYCKLAR-FEL TO TRUE                                            
069503       MOVE ZERO TO W-IDARTNR                                             
069603     END-IF                                                               
069703     IF NYCKLAR-OK                                                        
069803       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
069903       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
070003     ELSE                                                                 
070103       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
070203     END-IF                                                               
070303                                                                          
070403*    -- KONTROLL AV IDDC                                                  
070503     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
070603     IF EGEN-MID                                                          
070703       IF MID-IDDC-IN NOT = ALL '+'                                       
070803         MOVE '7'   TO MFS-IDPFK                                          
070903         MOVE SPACE TO MFS-KDTRTYP                                        
071003         MOVE MID-IDDC-IN TO W-IDDC-B6                                    
071103         PERFORM IMS-GU-WDB601                                            
071203         IF SEGMENT-FINNS                                                 
071303             MOVE MID-IDDC-IN TO MOD-IDDC-UT                              
071403                                 W-IDDC                                   
071503                                 SPAR-IDDC-ENTER                          
071603********                                                                  
071703             MOVE SPACE TO MOD-SHOW-UT                                    
071803                           MOD-TYPE-UT                                    
071903********                                                                  
072003         ELSE                                                             
072103           MOVE NEJ TO NYCKLAR-SW                                         
072203         END-IF                                                           
072303       ELSE                                                               
072403         IF MID-IDDC-UT NOT = ALL '+' OR SPACE                            
072503           MOVE MID-IDDC-UT TO W-IDDC-B6                                  
072603           PERFORM IMS-GU-WDB601                                          
072703           IF SEGMENT-FINNS                                               
072803               MOVE MID-IDDC-UT TO MOD-IDDC-UT                            
072903                                   W-IDDC                                 
073003                                   SPAR-IDDC-ENTER                        
073103           ELSE                                                           
073203             MOVE NEJ TO NYCKLAR-SW                                       
073303           END-IF                                                         
073403         ELSE                                                             
073503           MOVE MSGI-IDDC     TO W-IDDC                                   
073603                                 MOD-IDDC-UT                              
073703                                 SPAR-IDDC-ENTER                          
073803         END-IF                                                           
073903       END-IF                                                             
074003     ELSE                                                                 
074103       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
074203       PERFORM IMS-GU-WDB601                                              
074303       IF SEGMENT-FINNS                                                   
074403         MOVE MSGI-IDDC     TO W-IDDC                                     
074503                               MOD-IDDC-UT                                
074603                               PERS-WS-IDDC                               
074703                               SPAR-IDDC-ENTER                            
074803       ELSE                                                               
074903         MOVE NEJ TO NYCKLAR-SW                                           
075003       END-IF                                                             
075103     END-IF                                                               
075203     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
075303                                                                          
075403*    -- KONTROLL AV SHOW ALL                                              
075503     MOVE MFS-RENSA-FAELT TO MOD-SHOW-IN                                  
075603                                                                          
075703     IF EGEN-MID                                                          
075803       IF MID-SHOW-IN NOT = ALL '+'                                       
075903         MOVE '7'   TO MFS-IDPFK                                          
076003         MOVE SPACE TO MFS-KDTRTYP                                        
076103         IF MID-SHOW-IN = 'Y' OR 'J'                                      
076203           MOVE 'Y'          TO WS-SHOW                                   
076303                                MOD-SHOW-UT                               
076403         ELSE                                                             
076503           MOVE 'N'          TO WS-SHOW                                   
076603                                MOD-SHOW-UT                               
076703         END-IF                                                           
076803       ELSE                                                               
076903         IF MFS-FIRST                                                     
077003           IF W-IDDC = 11                                                 
077103             MOVE 'N'        TO WS-SHOW                                   
077203                                MOD-SHOW-UT                               
077303           ELSE                                                           
077403             MOVE 'Y'        TO WS-SHOW                                   
077503                                MOD-SHOW-UT                               
077603           END-IF                                                         
077703         ELSE                                                             
077803           MOVE MID-SHOW-UT  TO WS-SHOW                                   
077903                                MOD-SHOW-UT                               
078003         END-IF                                                           
078103       END-IF                                                             
078203     ELSE                                                                 
078303       IF W-IDDC = 11                                                     
078403         MOVE 'N'            TO WS-SHOW                                   
078503                                MOD-SHOW-UT                               
078603       ELSE                                                               
078703         MOVE 'Y'            TO WS-SHOW                                   
078803                                MOD-SHOW-UT                               
078903       END-IF                                                             
079003     END-IF                                                               
079103                                                                          
079203*    -- KONTROLL AV TYPE                                                  
079303     MOVE MFS-RENSA-FAELT TO MOD-TYPE-IN                                  
079403                                                                          
079503     IF EGEN-MID                                                          
079603       IF MID-TYPE-IN NOT = ALL '+'                                       
079703         MOVE '7'   TO MFS-IDPFK                                          
079803         MOVE SPACE TO MFS-KDTRTYP                                        
079903         IF MID-TYPE-IN = 'N'                                             
080003             MOVE MID-TYPE-IN  TO WS-TYPE                                 
080103                                  MOD-TYPE-UT                             
080203         ELSE                                                             
080303           MOVE NEJ TO NYCKLAR-SW                                         
080403         END-IF                                                           
080503       ELSE                                                               
080603           MOVE MID-TYPE-UT      TO WS-TYPE                               
080703                                    MOD-TYPE-UT                           
080803       END-IF                                                             
080903     END-IF                                                               
081003     IF GODK-MID OR NYCKLAR-OK                                            
081103       MOVE MSGI-IDDC TO PERS-WS-IDDC                                     
081203*                                                                         
081303       MOVE W-IDSKYLT TO MOD-IDSKYLT-UT                                   
081403*                                                                         
081503     ELSE                                                                 
081603       MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-UT                             
081703     END-IF                                                               
081803                                                                          
081903     IF NYCKLAR-FEL                                                       
082003       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
082103       CALL WMEDKONV USING MED-WMEDAREA                                   
082203       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
082303       PERFORM MFS-RENSA-FAELT-IN-CDC                                     
082403       PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                                 
082503       PERFORM MFS-RENSA-FAELT-UT                                         
082603     END-IF                                                               
082703     .                                                                    
082803     EJECT                                                                
082903 C-FOERSTA-SIDA   SECTION.                                                
083003     SKIP2                                                                
083103     PERFORM MFS-RENSA-FAELT-IN-CDC                                       
083203     PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                                   
083303     MOVE SPACE TO SPAR-IDDC2                                             
083403                   SPAR-IDDC-LEVEL1                                       
083503                   SPAR-IDDC-LEVEL2                                       
083603                   SPAR-IDDC-LEVEL3                                       
083703                   SPAR-IDDC-LEVEL1-ENTER                                 
083803                   SPAR-IDDC-LEVEL2-ENTER                                 
083903                   SPAR-IDDC-LEVEL3-ENTER                                 
084003                   SPAR-IDDC2-ENTER                                       
084103*    MOVE MSGI-IDDC  TO W-IDDC                                            
084203     MOVE SPAR-IDDC-ENTER TO W-IDDC                                       
084303     .                                                                    
084403     EJECT                                                                
084503 D-NAESTA-SIDA SECTION.                                                   
084603                                                                          
084703     IF SPAR-IDTRANS = '6308'                                             
084803        IF SPAR-IDARTNR-ENTER NUMERIC                                     
084903               MOVE SPAR-IDARTNR-ENTER       TO W-IDARTNR                 
085003        END-IF                                                            
085103     END-IF                                                               
085203     .                                                                    
085303     EJECT                                                                
085403                                                                          
085503 E-BEHANDLA-ENTER SECTION.                                                
085603     SKIP2                                                                
085703                                                                          
085803     IF EGEN-MID OR HELP-MID                                              
085903       IF MID-INPUT = ALL '+'                                             
086003         SET INDATA-SAKNAS TO TRUE                                        
086103         IF MID-IDDC-IN = ALL '+' OR SPACE                                
086203           IF SPAR-IDDC-ENTER NOT = SPACE                                 
086303             MOVE SPAR-IDDC-ENTER      TO W-IDDC                          
086403*                                           MOD-IDDC-UT                   
086503           ELSE                                                           
086603             MOVE MSGI-IDDC            TO W-IDDC                          
086703                                            MOD-IDDC-UT                   
086803                                            SPAR-IDDC-ENTER               
086903           END-IF                                                         
087003         END-IF                                                           
087103         MOVE SPACE                    TO SPAR-IDDC2                      
087203                                        SPAR-IDDC-LEVEL1                  
087303                                        SPAR-IDDC-LEVEL2                  
087403                                        SPAR-IDDC-LEVEL3                  
087503         PERFORM MFS-RENSA-FAELT-IN-CDC                                   
087603         PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                               
087703       ELSE                                                               
087803         SET INDATA-FINNS TO TRUE                                         
087903           MOVE SPAR-IDDC2-ENTER     TO SPAR-IDDC2                        
088003           MOVE SPAR-IDDC-LEVEL1-ENTER TO SPAR-IDDC-LEVEL1                
088103           IF MID-IDDC-IN NOT = ALL '+'                                   
088203             IF MID-IDDC-IN = '11'                                        
088303               MOVE SPAR-IDDC-LEVEL2-ENTER TO SPAR-IDDC-LEVEL2            
088403             ELSE                                                         
088503               MOVE MID-IDDC-IN TO SPAR-IDDC-LEVEL2                       
088603             END-IF                                                       
088703           ELSE                                                           
088803             MOVE SPAR-IDDC-LEVEL2-ENTER TO SPAR-IDDC-LEVEL2              
088903           END-IF                                                         
089003           MOVE SPAR-IDDC-LEVEL3-ENTER TO SPAR-IDDC-LEVEL3                
089103         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
089203         CALL WMEDKONV USING MED-WMEDAREA                                 
089303         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
089403         PERFORM EA-INDATA-TILL-MOD                                       
089503       END-IF                                                             
089603     ELSE                                                                 
089703       PERFORM MFS-RENSA-FAELT-IN-CDC                                     
089803       PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                                 
089903     END-IF                                                               
090003     .                                                                    
090103     EJECT                                                                
090203                                                                          
090303 EA-INDATA-TILL-MOD SECTION.                                              
090403     SKIP2                                                                
090503     IF MID-TEARTNOT-GLOB NOT = ALL '+'                                   
090603       MOVE MID-TEARTNOT-GLOB TO MOD-TEARTNOT-GLOB                        
090703       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEARTNOT-GLOB-ATTR               
090803     ELSE                                                                 
090903       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-GLOB                          
091003     END-IF                                                               
091103                                                                          
091203     IF MID-KDLEVSP-GLOB NOT = ALL '+'                                    
091303       MOVE MID-KDLEVSP-GLOB TO MOD-KDLEVSP-GLOB-UPD                      
091403       INSPECT MOD-KDLEVSP-GLOB-UPD REPLACING LEADING ' ' BY '0'          
091503       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLEVSP-GLOB-UPD-ATTR            
091603     ELSE                                                                 
091703       MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-GLOB-UPD                       
091803     END-IF                                                               
091903                                                                          
092003     IF MID-KDLEVSP-DC11 NOT = ALL '+'                                    
092103       MOVE MID-KDLEVSP-DC11 TO MOD-KDLEVSP-DC11-UPD                      
092203       INSPECT MOD-KDLEVSP-DC11-UPD REPLACING LEADING ' ' BY '0'          
092303       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLEVSP-DC11-UPD-ATTR            
092403     ELSE                                                                 
092503       MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-DC11-UPD                       
092603     END-IF                                                               
092703                                                                          
092803     IF MID-KVSPARR-KVAL-DC11 NOT = ALL '+'                               
092903       MOVE MID-KVSPARR-KVAL-DC11 TO MOD-KVSPARR-KVAL-DC11-UPD            
093003       INSPECT MOD-KVSPARR-KVAL-DC11-UPD                                  
093103                                    REPLACING LEADING ' ' BY '0'          
093203       MOVE MFS-ADD-LAES-IN-FAELT                                         
093303                            TO MOD-KVSPARR-KVAL-DC11-UPD-ATTR             
093403     ELSE                                                                 
093503       MOVE MFS-RENSA-FAELT TO MOD-KVSPARR-KVAL-DC11-UPD                  
093603     END-IF                                                               
093703                                                                          
093803     MOVE +1 TO RAD-IX                                                    
093903     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
094003       IF MID-KDLEVSP(RAD-IX) NOT = ALL '+'                               
094103         MOVE MID-KDLEVSP(RAD-IX) TO MOD-KDLEVSP-UPD(RAD-IX)              
094203         INSPECT MOD-KDLEVSP-UPD(RAD-IX)                                  
094303                          REPLACING LEADING ' ' BY '0'                    
094403         MOVE MFS-ADD-LAES-IN-FAELT                                       
094503                          TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)                 
094603       ELSE                                                               
094703         MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-UPD(RAD-IX)                  
094803       END-IF                                                             
094903                                                                          
095003       IF MID-KVSPARR-KVAL(RAD-IX) NOT = ALL '+'                          
095103         MOVE MID-KVSPARR-KVAL(RAD-IX)                                    
095203                              TO MOD-KVSPARR-KVAL-UPD(RAD-IX)             
095303         INSPECT MOD-KVSPARR-KVAL-UPD(RAD-IX)                             
095403                                      REPLACING LEADING ' ' BY '0'        
095503         MOVE MFS-ADD-LAES-IN-FAELT                                       
095603                              TO MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)        
095703       ELSE                                                               
095803         MOVE MFS-RENSA-FAELT TO MOD-KVSPARR-KVAL-UPD(RAD-IX)             
095903       END-IF                                                             
096003                                                                          
096103       IF MID-TEKVAL(RAD-IX) NOT = ALL '+'                                
096203         MOVE MID-TEKVAL(RAD-IX) TO MOD-TEKVAL(RAD-IX)                    
096303         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAL-ATTR(RAD-IX)            
096403       ELSE                                                               
096503         MOVE MFS-RENSA-FAELT TO MOD-TEKVAL(RAD-IX)                       
096603       END-IF                                                             
096703       ADD +1 TO RAD-IX                                                   
096803     END-PERFORM                                                          
096903                                                                          
097003     .                                                                    
097103     EJECT                                                                
097203                                                                          
097303                                                                          
097403 F-LAES-VISA-INFO SECTION.                                                
097503     SKIP2                                                                
097603     PERFORM IMS-GET-BENA01-BSEQ                                          
097703     IF SEGMENT-FINNS                                                     
097803       PERFORM IMS-GET-BENA11                                             
097903       MOVE BENA-TEXT-BEART TO MOD-BEART                                  
098003       PERFORM IMS-GET-UNIK-WLARTC01                                      
098103*      --- Här abendas om man inte får träff på ARTC01                    
098203     ELSE                                                                 
098303       MOVE MFS-RENSA-FAELT  TO MOD-BEART                                 
098403     END-IF                                                               
098503     IF SEGMENT-SAKNAS                                                    
098603*      --- Benämning fanns inte för denna artikel                         
098703*      --- Alltså finns inte artikeln heller                              
098803       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
098903       CALL WMEDKONV USING MED-WMEDAREA                                   
099003       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
099103       PERFORM MFS-RENSA-FAELT-UT                                         
099203     ELSE                                                                 
099303*      --- Kolla först om det är en "rensad" art. ARTC01 är läst          
099403       MOVE '11'     TO MOD-IDDC-DC11                                     
099503       IF ARTC-ART-KDERS-UTG = Zero                                       
099603*      --- OK,då börjar visnings-karusellen.                              
099703         PERFORM IMS-GET-NEXT-WLARTC11                                    
099803         IF SEGMENT-FINNS                                                 
099903           IF ARTC-CLAG-KDLEVSP = 20                                      
100003*            -- Globalspärrat - Visa i GLOB-fälten                        
100103             MOVE ARTC-CLAG-KDLEVSP     TO MOD-KDLEVSP-GLOB               
100203                                             MOD-KDLEVSP-DC11             
100303            MOVE ARTC-CLAG-TISPARR-KVAL TO MOD-TISPARR-KVAL-DC11          
100403            MOVE ARTC-CLAG-IDUSER-SPKVAL to MOD-IDUSER-SPKVAL-DC11        
100503           ELSE                                                           
100603             IF ARTC-CLAG-KDLEVSP = 21                                    
100703*              -- CDC-spärrat - Visa i CDC11-fälten                       
100803               MOVE ARTC-CLAG-KDLEVSP    TO MOD-KDLEVSP-DC11              
100903              MOVE ARTC-CLAG-TISPARR-KVAL TO MOD-TISPARR-KVAL-DC11        
101003               MOVE ARTC-CLAG-IDUSER-SPKVAL TO                            
101103                                           MOD-IDUSER-SPKVAL-DC11         
101203               MOVE MFS-RENSA-FAELT     TO MOD-KDLEVSP-GLOB               
101303             ELSE                                                         
101403*              -- CDC-NOLLAT - Visa i CDC11-fälten                        
101503               MOVE ARTC-CLAG-KDLEVSP    TO MOD-KDLEVSP-DC11              
101603              MOVE ARTC-CLAG-TISPARR-KVAL TO MOD-TISPARR-KVAL-DC11        
101703               MOVE ARTC-CLAG-IDUSER-SPKVAL TO                            
101803                                           MOD-IDUSER-SPKVAL-DC11         
101903               MOVE MFS-RENSA-FAELT      TO MOD-KDLEVSP-GLOB              
102003             END-IF                                                       
102103           END-IF                                                         
102203           MOVE ARTC-CLAG-KVSPARR-KVAL   TO MOD-KVSPARR-KVAL-DC11         
102303           MOVE ARTC-CLAG-kvls           TO MOD-kvls-dc11                 
102403           IF ARTC-CLAG-TIMAIL-KVAL NOT = ZERO                            
102503              MOVE ARTC-CLAG-TIMAIL-KVAL TO MOD-TIMAIL-KVAL               
102603           ELSE                                                           
102703              MOVE MFS-RENSA-FAELT       TO MOD-TIMAIL-KVAL               
102803           END-IF                                                         
102903                                                                          
103003           MOVE NEJ TO SW-TEXT-FINNS                                      
103103           PERFORM IMS-GET-W6D201                                         
103203           IF SEGMENT-FINNS                                               
103303              PERFORM IMS-GET-W6D211                                      
103403              IF SEGMENT-FINNS                                            
103503                 IF INFO-IDKVAINF = 99                                    
103603                    PERFORM IMS-GET-W6D211                                
103703                 END-IF                                                   
103803              END-IF                                                      
103903              IF SEGMENT-FINNS                                            
104003                 MOVE +1 TO IX                                            
104103                 PERFORM UNTIL IX > IX-MAX OR (SW-TEXT-FINNS = JA)        
104203                    IF INFO-TEKVAINF-EXT(IX) NOT = SPACE                  
104303                       MOVE JA TO SW-TEXT-FINNS                           
104403                    END-IF                                                
104503                    ADD +1 TO IX                                          
104603                 END-PERFORM                                              
104703              END-IF                                                      
104803           END-IF                                                         
104903                                                                          
105003           IF SW-TEXT-FINNS = JA                                          
105103              MOVE 'Y'          TO MOD-FLTEXT                             
105203           ELSE                                                           
105303             MOVE MFS-RENSA-FAELT TO MOD-FLTEXT                           
105403           END-IF                                                         
105503                                                                          
105603           IF INDATA-FINNS AND MID-TEARTNOT-GLOB NOT = ALL '+'            
105703             CONTINUE                                                     
105803           ELSE                                                           
105903             PERFORM IMS-GET-WLARTC25                                     
106003*            --- Läser med KDNOTTYP = 4                                   
106103             IF SEGMENT-FINNS                                             
106203               MOVE ARTC-NOT-TEARTNOT TO MOD-TEARTNOT-GLOB                
106303             ELSE                                                         
106403               MOVE MFS-RENSA-FAELT   TO MOD-TEARTNOT-GLOB                
106503             END-IF                                                       
106603           END-IF                                                         
106703                                                                          
106803*          --- Läs WDK7 SDC/NDC                                           
106903           PERFORM FB-BESTAM-LEVEL                                        
107003           EVALUATE WS-LEVEL                                              
107103             WHEN ZERO                                                    
107203***EJ   I DC TREE BILD 6315 VISAR BARA DC11 OCH INMATAT DC                
107303               PERFORM FBA-LAES-ARTS11-EJLEVEL                            
107403             WHEN +1                                                      
107503***LEVEL1   ALLTID DC11 VISAR DC11 OCH ALLA LEVEL 2 DC                    
107603               PERFORM FBB-LAES-ARTS11-LEVEL1                             
107703             WHEN +2                                                      
107803***LEVEL2   VISAR DC11 AKTUELLT LEVEL2 DC OCH ALLA LEVEL3 DC SOM          
107903***LIGGER   UNDER AKTUELLT LEVEL2 DC                                      
108003               PERFORM FBC-LAES-ARTS11-LEVEL2                             
108103             WHEN +3                                                      
108203***LEVEL3   VISAR DC11 AKTUELLT LEVEL3 DC SAMT DESS LEVEL2 DC             
108303               PERFORM FBD-LAES-ARTS11-LEVEL3                             
108403             WHEN +4                                                      
108503***VISAR   DC11 OCH ALLA DC UPPLAGDA I DC TREE PÅ BILD 6351               
108603               PERFORM FBE-LAES-ARTS11-SHOWALL                            
108703             WHEN +5                                                      
108803***VISAR   DC11 OCH ALLA LEVEL2 SOM HAR 'N' I KDDC(WDR2,WDGX6332)         
108903               PERFORM FBF-LAES-ARTS11-TYPE                               
109003           END-EVALUATE                                                   
109103           IF INDATA-OK                                                   
109203              MOVE MFS-ADD-SET-CURSOR TO MOD-IDARTNR-IN-ATTR              
109303           END-IF                                                         
109403         ELSE                                                             
109503           PERFORM MFS-RENSA-FAELT-UT-CDC                                 
109603           PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                             
109703           PERFORM MFS-RENSA-FAELT-IN-UT-SDC-NDC                          
109803           MOVE MFS-RENSA-FAELT TO MOD-TIMAIL-KVAL                        
109903         END-IF                                                           
110003       ELSE                                                               
110103         PERFORM MFS-RENSA-FAELT-UT-CDC                                   
110203         PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                               
110303         PERFORM MFS-RENSA-FAELT-IN-UT-SDC-NDC                            
110403         MOVE MFS-RENSA-FAELT TO MOD-TIMAIL-KVAL                          
110503       END-IF                                                             
110603     END-IF                                                               
110703                                                                          
110803     IF INDATA-OK                                                         
110903       MOVE MFS-ADD-SET-CURSOR TO MOD-IDARTNR-IN-ATTR                     
111003     END-IF                                                               
111103                                                                          
111203*    --- RENSA alla inmatningsfält ifall ingen indata finns               
111303     IF INDATA-SAKNAS                                                     
111403       PERFORM MFS-RENSA-FAELT-IN-CDC                                     
111503       PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                                 
111603     END-IF                                                               
111703                                                                          
111803     MOVE WS-SHOW    TO SPAR-SHOW-ENTER                                   
111903     MOVE WS-TYPE    TO SPAR-TYPE-ENTER                                   
112003     MOVE '002'      TO MSGI-KDCALL                                       
112103     MOVE '6308'     TO SPAR-IDTRANS                                      
112203     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
112303     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
112403*    --- ÖPPNA inmatningsfält FÖR CDC                                     
112503     IF PERS-CDC                                                          
112603         MOVE MFS-OPEN-NUM-FIELD TO MOD-KDLEVSP-GLOB-UPD-ATTR             
112703                                    MOD-KDLEVSP-DC11-UPD-ATTR             
112803                                    MOD-KVSPARR-KVAL-DC11-UPD-ATTR        
112903     END-IF                                                               
113003     .                                                                    
113103     EJECT                                                                
113203                                                                          
113303 FA-LAES-ARTS11 SECTION.                                                  
113403     SKIP2                                                                
113503     PERFORM IMS-GET-UNIK-WLARTS01                                        
113603     IF SEGMENT-FINNS                                                     
113703       MOVE WS-IDDC-TREE TO W-IDDC                                        
113803       PERFORM IMS-GET-UNIK-WLARTS11                                      
113903                                                                          
114003       IF SEGMENT-FINNS                                                   
114103         MOVE ARTS-SLAG-IDDC    TO WS-IDDC                                
114203                                   MOD-IDDC(RAD-IX)                       
114303                                   SPAR-IDDC-TAB(RAD-IX)                  
114403         MOVE ARTS-SLAG-KDLEVSP          TO MOD-KDLEVSP(RAD-IX)           
114503         MOVE ARTS-SLAG-KVSPARR-KVAL TO                                   
114603                                MOD-KVSPARR-KVAL(RAD-IX)                  
114703                                                                          
114803         IF      MOD-TEKVAL-ATTR(RAD-IX) = MFS-ADD-LAES-IN-FAELT          
114903         AND PERS-SDC-NL                                                  
115003           CONTINUE                                                       
115103         ELSE                                                             
115203           MOVE ARTS-SLAG-TEKVAL TO MOD-TEKVAL(RAD-IX)                    
115303         END-IF                                                           
115403         IF PERS-CDC                                                      
115503           IF W-IDUSER1 = MSGI-IDUSER                                     
115603           OR W-IDUSER2 = MSGI-IDUSER                                     
115703           OR W-IDUSER3 = MSGI-IDUSER                                     
115803           OR W-IDUSER4 = MSGI-IDUSER                                     
115903             CONTINUE                                                     
116003           ELSE                                                           
116103             MOVE MFS-CLOSE-FIELD TO                                      
116203                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
116303             MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                           
116403           END-IF                                                         
116503         END-IF                                                           
116603                                                                          
116703         MOVE ARTS-SLAG-IDUSER-SPKVAL                                     
116803                                   TO MOD-IDUSER-SPKVAL(RAD-IX)           
116903         MOVE ARTS-SLAG-TISPARR-KVAL TO MOD-TISPARR-KVAL(RAD-IX)          
117003         MOVE ARTS-SLAG-KVLS             TO MOD-KVLS(RAD-IX)              
117103       END-IF                                                             
117203     END-IF                                                               
117303     .                                                                    
117403     EJECT                                                                
117503 FAB-LAES-ARTS11 SECTION.                                                 
117603     SKIP2                                                                
117703                                                                          
117803     IF SEGMENT-FINNS                                                     
117903       IF SPAR-IDDC2 NOT = SPACE AND RAD-IX = 1                           
118003         MOVE SPAR-IDDC2 TO W-IDDC2                                       
118103                            SPAR-IDDC2-ENTER                              
118203         PERFORM IMS-GET-NEXT-WLARTS11-2                                  
118303       ELSE                                                               
118403         PERFORM IMS-GET-NEXT-WLARTS11                                    
118503       END-IF                                                             
118603       IF SEGMENT-SAKNAS                                                  
118703         ADD -1 TO RAD-IX                                                 
118803       ELSE                                                               
118903         MOVE ARTS-SLAG-IDDC    TO WS-IDDC                                
119003                                 MOD-IDDC(RAD-IX)                         
119103                                 SPAR-IDDC-TAB(RAD-IX)                    
119203         MOVE ARTS-SLAG-KDLEVSP          TO MOD-KDLEVSP(RAD-IX)           
119303         MOVE ARTS-SLAG-KVSPARR-KVAL TO                                   
119403                                MOD-KVSPARR-KVAL(RAD-IX)                  
119503                                                                          
119603         IF      MOD-TEKVAL-ATTR(RAD-IX) = MFS-ADD-LAES-IN-FAELT          
119703         AND PERS-SDC-NL                                                  
119803           CONTINUE                                                       
119903         ELSE                                                             
120003           MOVE ARTS-SLAG-TEKVAL TO MOD-TEKVAL(RAD-IX)                    
120103         END-IF                                                           
120203                                                                          
120303         MOVE ARTS-SLAG-IDUSER-SPKVAL                                     
120403                                   TO MOD-IDUSER-SPKVAL(RAD-IX)           
120503         MOVE ARTS-SLAG-TISPARR-KVAL TO MOD-TISPARR-KVAL(RAD-IX)          
120603         MOVE ARTS-SLAG-KVLS             TO MOD-KVLS(RAD-IX)              
120703         MOVE MFS-CLOSE-FIELD TO                                          
120803                              MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)           
120903*                             MOD-KDLEVSP-UPD-ATTR(RAD-IX)                
121003         MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                               
121103       END-IF                                                             
121203     END-IF                                                               
121303     .                                                                    
121403     EJECT                                                                
121503                                                                          
121603 FB-BESTAM-LEVEL  SECTION.                                                
121703                                                                          
121803     PERFORM IMS-GU-WDGX6331                                              
121903     IF SEGMENT-FINNS                                                     
122003       IF SPAR-IDDC-ENTER = '11' AND                                      
122103          (WS-SHOW = 'Y' OR WS-TYPE = 'N')                                
122203         IF WS-SHOW = 'Y'                                                 
122303           MOVE +4 TO WS-LEVEL                                            
122403         ELSE                                                             
122503           IF WS-TYPE = 'N'                                               
122603             MOVE +5 TO WS-LEVEL                                          
122703           END-IF                                                         
122803         END-IF                                                           
122903       ELSE                                                               
123003         IF SPAR-IDDC-ENTER  = '11'                                       
123103           MOVE +1 TO WS-LEVEL                                            
123203         ELSE                                                             
123303           MOVE SPAR-IDDC-ENTER TO W-6332-IDDC                            
123403           PERFORM IMS-GNP-WDGX6332                                       
123503           IF SEGMENT-FINNS                                               
123603             MOVE +2 TO WS-LEVEL                                          
123703           ELSE                                                           
123803             MOVE SPAR-IDDC-ENTER TO W-6334-IDDC                          
123903             PERFORM IMS-GU-WDGX6331                                      
124003             PERFORM IMS-GNP-WDGX6334-2                                   
124103             IF SEGMENT-FINNS                                             
124203               MOVE +3 TO WS-LEVEL                                        
124303             ELSE                                                         
124403               MOVE ZERO TO WS-LEVEL                                      
124503             END-IF                                                       
124603           END-IF                                                         
124703         END-IF                                                           
124803       END-IF                                                             
124903     END-IF                                                               
125003     .                                                                    
125103     EJECT                                                                
125203                                                                          
125303 FBA-LAES-ARTS11-EJLEVEL SECTION.                                         
125403                                                                          
125503     MOVE SPAR-IDDC-ENTER TO WS-IDDC-TREE                                 
125603     MOVE +1   TO RAD-IX                                                  
125703     PERFORM FA-LAES-ARTS11                                               
125803     IF SEGMENT-FINNS                                                     
125903       IF PERS-WS-IDDC NOT = MOD-IDDC(RAD-IX)                             
126003         IF W-IDUSER1 = MSGI-IDUSER                                       
126103         OR W-IDUSER2 = MSGI-IDUSER                                       
126203         OR W-IDUSER3 = MSGI-IDUSER                                       
126303         OR W-IDUSER4 = MSGI-IDUSER                                       
126403           CONTINUE                                                       
126503         ELSE                                                             
126603           IF PERS-CDC                                                    
126703             MOVE MFS-CLOSE-FIELD TO                                      
126803                             MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)            
126903             MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                           
127003           ELSE                                                           
127103             MOVE MFS-CLOSE-FIELD TO                                      
127203                             MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)            
127303                             MOD-KDLEVSP-UPD-ATTR(RAD-IX)                 
127403             MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                           
127503           END-IF                                                         
127603         END-IF                                                           
127703       END-IF                                                             
127803       ADD  +1   TO RAD-IX                                                
127903     END-IF                                                               
128003     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
128103       MOVE MFS-STAENG-FAELT TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)              
128203                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
128303                                MOD-TEKVAL-ATTR(RAD-IX)                   
128403       MOVE SPACE            TO SPAR-IDDC-TAB(RAD-IX)                     
128503       ADD +1  TO RAD-IX                                                  
128603     END-PERFORM                                                          
128703     .                                                                    
128803     EJECT                                                                
128903                                                                          
129003 FBB-LAES-ARTS11-LEVEL1 SECTION.                                          
129103                                                                          
129203*    --- Rensa först alla utdatafält                                      
129303     PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                                   
129403     PERFORM IMS-GU-WDGX6331                                              
129503     IF SPAR-IDDC-LEVEL2 NOT = SPACE                                      
129603       MOVE SPAR-IDDC-LEVEL2     TO W-IDDC-6332                           
129703                                    SPAR-IDDC-LEVEL2-ENTER                
129803       PERFORM IMS-GNP-WDGX6332                                           
129903       MOVE 6332-IDUSER(1) TO W-IDUSER1                                   
130003       MOVE 6332-IDUSER(2) TO W-IDUSER2                                   
130103       MOVE 6332-IDUSER(3) TO W-IDUSER3                                   
130203       MOVE 6332-IDUSER(4) TO W-IDUSER4                                   
130303     ELSE                                                                 
130403       PERFORM IMS-GNP-WDGX6332-2                                         
130503       MOVE 6332-IDUSER(1) TO W-IDUSER1                                   
130603       MOVE 6332-IDUSER(2) TO W-IDUSER2                                   
130703       MOVE 6332-IDUSER(3) TO W-IDUSER3                                   
130803       MOVE 6332-IDUSER(4) TO W-IDUSER4                                   
130903     END-IF                                                               
131003     MOVE +1   TO RAD-IX                                                  
131103     PERFORM UNTIL RAD-IX > RAD-IX-MAX OR SEGMENT-SAKNAS                  
131203       MOVE 6332-IDDC    TO WS-IDDC-TREE                                  
131303       PERFORM FA-LAES-ARTS11                                             
131403       IF SEGMENT-FINNS                                                   
131503         IF PERS-WS-IDDC NOT = MOD-IDDC(RAD-IX)                           
131603           IF W-IDUSER1 = MSGI-IDUSER                                     
131703           OR W-IDUSER2 = MSGI-IDUSER                                     
131803           OR W-IDUSER3 = MSGI-IDUSER                                     
131903           OR W-IDUSER4 = MSGI-IDUSER                                     
132003             CONTINUE                                                     
132103           ELSE                                                           
132203             IF PERS-CDC                                                  
132303               MOVE MFS-CLOSE-FIELD TO                                    
132403                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
132503               MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                         
132603             ELSE                                                         
132703               MOVE MFS-CLOSE-FIELD TO                                    
132803                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
132903                               MOD-KDLEVSP-UPD-ATTR(RAD-IX)               
133003               MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                         
133103             END-IF                                                       
133203           END-IF                                                         
133303         END-IF                                                           
133403         PERFORM FBBA-SUMMERA-LVL3                                        
133503         ADD +1 TO RAD-IX                                                 
133603       END-IF                                                             
133703                                                                          
133803       PERFORM IMS-GNP-WDGX6332-2                                         
133903       MOVE 6332-IDUSER(1) TO W-IDUSER1                                   
134003       MOVE 6332-IDUSER(2) TO W-IDUSER2                                   
134103       MOVE 6332-IDUSER(3) TO W-IDUSER3                                   
134203       MOVE 6332-IDUSER(4) TO W-IDUSER4                                   
134303     END-PERFORM                                                          
134403     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
134503       MOVE MFS-STAENG-FAELT TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)              
134603                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
134703                                MOD-TEKVAL-ATTR(RAD-IX)                   
134803       MOVE SPACE            TO SPAR-IDDC-TAB(RAD-IX)                     
134903       ADD +1  TO RAD-IX                                                  
135003     END-PERFORM                                                          
135103     IF SEGMENT-FINNS                                                     
135203       MOVE 6332-IDDC         TO SPAR-IDDC-LEVEL2                         
135303       MOVE INF-MORE-INFO-EXISTS         TO MED-IDMFSINF                  
135403       CALL WMEDKONV USING MED-WMEDAREA                                   
135503       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
135603     ELSE                                                                 
135703       MOVE 'THIS IS LAST PAGE' TO MOD-TEMFSINF                           
135803       MOVE SPACE             TO SPAR-IDDC-LEVEL2                         
135903     END-IF                                                               
136003     .                                                                    
136103     EJECT                                                                
136203                                                                          
136303 FBBA-SUMMERA-LVL3 SECTION.                                               
136403                                                                          
136503     MOVE SPACE TO MOD-FLDCLVL3(RAD-IX)                                   
136603                   MOD-FLSPARR (RAD-IX)                                   
136703                                                                          
136803     MOVE MOD-KVSPARR-KVAL(RAD-IX) TO W-ART-KVSPANT                       
136903     MOVE MOD-KVLS        (RAD-IX) TO W-ART-KVLS                          
137003                                                                          
137103     MOVE 6332-IDDC TO W-6332-IDDC                                        
137203     PERFORM IMS-GNP-WDGX6334-32                                          
137303     PERFORM UNTIL SEGMENT-SAKNAS                                         
137403       MOVE 'Y'   TO MOD-FLDCLVL3(RAD-IX)                                 
137503       MOVE 6334-IDDC  TO W-IDDC                                          
137603       PERFORM IMS-GET-UNIK-WLARTS11                                      
137703       IF SEGMENT-FINNS                                                   
137803         IF ARTS-SLAG-KDLEVSP > ZERO                                      
137903           MOVE 'Y' TO MOD-FLSPARR(RAD-IX)                                
138003         END-IF                                                           
138103         ADD ARTS-SLAG-KVSPARR-KVAL TO W-ART-KVSPANT                      
138203         ADD ARTS-SLAG-KVLS         TO W-ART-KVLS                         
138303       END-IF                                                             
138403       PERFORM IMS-GNP-WDGX6334-32                                        
138503     END-PERFORM                                                          
138603     MOVE W-ART-KVSPANT TO MOD-KVSPARR-KVAL(RAD-IX)                       
138703     MOVE W-ART-KVLS    TO MOD-KVLS        (RAD-IX)                       
138803     .                                                                    
138903     EJECT                                                                
139003                                                                          
139103 FBC-LAES-ARTS11-LEVEL2 SECTION.                                          
139203                                                                          
139303*    --- Rensa först alla utdatafält                                      
139403     PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                                   
139503     IF SPAR-IDDC-LEVEL2 NOT = SPACE                                      
139603       MOVE SPAR-IDDC-LEVEL2     TO W-6332-IDDC                           
139703                                    WS-IDDC-TREE                          
139803                                    SPAR-IDDC-LEVEL2-ENTER                
139903     ELSE                                                                 
140003       MOVE SPAR-IDDC-ENTER      TO W-6332-IDDC                           
140103                                    WS-IDDC-TREE                          
140203                                    SPAR-IDDC-LEVEL2-ENTER                
140303     END-IF                                                               
140403****LEVEL2*****                                                           
140503     PERFORM IMS-GU-WDGX6332                                              
140603     IF SEGMENT-FINNS                                                     
140703       MOVE 6332-IDDC      TO SPAR-IDDC-LEVEL2                            
140803       MOVE +1 TO RAD-IX                                                  
140903       MOVE 6332-IDUSER(1) TO W-IDUSER1                                   
141003       MOVE 6332-IDUSER(2) TO W-IDUSER2                                   
141103       MOVE 6332-IDUSER(3) TO W-IDUSER3                                   
141203       MOVE 6332-IDUSER(4) TO W-IDUSER4                                   
141303       PERFORM FA-LAES-ARTS11                                             
141403       IF SEGMENT-FINNS                                                   
141503         IF PERS-WS-IDDC NOT = MOD-IDDC(RAD-IX)                           
141603           IF W-IDUSER1 = MSGI-IDUSER                                     
141703           OR W-IDUSER2 = MSGI-IDUSER                                     
141803           OR W-IDUSER3 = MSGI-IDUSER                                     
141903           OR W-IDUSER4 = MSGI-IDUSER                                     
142003             MOVE JA TO WS-OPPNA                                          
142103           ELSE                                                           
142203             IF PERS-CDC                                                  
142303               MOVE MFS-CLOSE-FIELD TO                                    
142403                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
142503               MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                         
142603             ELSE                                                         
142703               MOVE MFS-CLOSE-FIELD TO                                    
142803                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
142903                               MOD-KDLEVSP-UPD-ATTR(RAD-IX)               
143003               MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                         
143103             END-IF                                                       
143203           END-IF                                                         
143303         ELSE                                                             
143403           MOVE JA TO WS-OPPNA                                            
143503         END-IF                                                           
143603****LEVEL3*****                                                           
143703         IF WS-SHOW = 'Y' OR ' '                                          
143803           ADD +1 TO RAD-IX                                               
143903           PERFORM FBCA-VISA-LVL2-3                                       
144003         ELSE                                                             
144103           PERFORM FBCB-VISA-LVL2-MED-SUM                                 
144203           ADD +1 TO RAD-IX                                               
144303         END-IF                                                           
144403       END-IF                                                             
144503                                                                          
144603       PERFORM UNTIL RAD-IX > RAD-IX-MAX                                  
144703         MOVE MFS-STAENG-FAELT TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)            
144803                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
144903                                MOD-TEKVAL-ATTR(RAD-IX)                   
145003         MOVE SPACE            TO SPAR-IDDC-TAB(RAD-IX)                   
145103         ADD +1 TO RAD-IX                                                 
145203       END-PERFORM                                                        
145303       IF SEGMENT-FINNS                                                   
145403         MOVE 6334-IDDC       TO SPAR-IDDC-LEVEL3                         
145503         MOVE INF-MORE-INFO-EXISTS       TO MED-IDMFSINF                  
145603         CALL WMEDKONV USING MED-WMEDAREA                                 
145703         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
145803       ELSE                                                               
145903         MOVE 'THIS IS LAST PAGE' TO MOD-TEMFSINF                         
146003         MOVE SPACE           TO SPAR-IDDC-LEVEL3                         
146103       END-IF                                                             
146203     END-IF                                                               
146303     .                                                                    
146403     EJECT                                                                
146503                                                                          
146603 FBCA-VISA-LVL2-3 SECTION.                                                
146703                                                                          
146803     IF SPAR-IDDC-LEVEL3 NOT = SPACE                                      
146903       MOVE SPAR-IDDC-LEVEL3 TO W-6334-IDDC                               
147003                                  SPAR-IDDC-LEVEL3-ENTER                  
147103       PERFORM IMS-GNP-WDGX6334-2                                         
147203     ELSE                                                                 
147303       PERFORM IMS-GNP-WDGX6334                                           
147403     END-IF                                                               
147503     PERFORM UNTIL RAD-IX > RAD-IX-MAX OR SEGMENT-SAKNAS                  
147603       MOVE 6334-IDDC  TO WS-IDDC-TREE                                    
147703       PERFORM FA-LAES-ARTS11                                             
147803       IF SEGMENT-FINNS                                                   
147903         IF WS-OPPNA = JA                                                 
148003         OR MSGI-IDDC = 6334-IDDC                                         
148103           CONTINUE                                                       
148203         ELSE                                                             
148303           IF PERS-CDC                                                    
148403             MOVE MFS-CLOSE-FIELD TO                                      
148503                             MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)            
148603             MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                           
148703           ELSE                                                           
148803             MOVE MFS-CLOSE-FIELD TO                                      
148903                             MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)            
149003                             MOD-KDLEVSP-UPD-ATTR(RAD-IX)                 
149103             MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                           
149203           END-IF                                                         
149303         END-IF                                                           
149403         ADD +1 TO RAD-IX                                                 
149503       END-IF                                                             
149603       PERFORM IMS-GNP-WDGX6334                                           
149703     END-PERFORM                                                          
149803     .                                                                    
149903     EJECT                                                                
150003 FBCB-VISA-LVL2-MED-SUM SECTION.                                          
150103                                                                          
150203     MOVE SPACE TO MOD-FLDCLVL3(RAD-IX)                                   
150303                   MOD-FLSPARR (RAD-IX)                                   
150403                                                                          
150503     MOVE MOD-KVSPARR-KVAL(RAD-IX) TO W-ART-KVSPANT                       
150603     MOVE MOD-KVLS        (RAD-IX) TO W-ART-KVLS                          
150703     PERFORM IMS-GNP-WDGX6334                                             
150803     PERFORM UNTIL SEGMENT-SAKNAS                                         
150903       MOVE 'Y'   TO MOD-FLDCLVL3(RAD-IX)                                 
151003       MOVE 6334-IDDC  TO W-IDDC                                          
151103       PERFORM IMS-GET-UNIK-WLARTS11                                      
151203       IF SEGMENT-FINNS                                                   
151303         IF ARTS-SLAG-KDLEVSP > ZERO                                      
151403           MOVE 'Y' TO MOD-FLSPARR(RAD-IX)                                
151503         END-IF                                                           
151603         ADD ARTS-SLAG-KVSPARR-KVAL TO W-ART-KVSPANT                      
151703         ADD ARTS-SLAG-KVLS         TO W-ART-KVLS                         
151803       END-IF                                                             
151903       PERFORM IMS-GNP-WDGX6334                                           
152003     END-PERFORM                                                          
152103     MOVE W-ART-KVSPANT TO MOD-KVSPARR-KVAL(RAD-IX)                       
152203     MOVE W-ART-KVLS    TO MOD-KVLS        (RAD-IX)                       
152303     .                                                                    
152403     EJECT                                                                
152503 FBD-LAES-ARTS11-LEVEL3 SECTION.                                          
152603                                                                          
152703*    --- Rensa först alla utdatafält                                      
152803     PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                                   
152903     MOVE 6332-KEY-FB-AREA-IDDC  TO W-6332-IDDC                           
153003                                    WS-IDDC-TREE                          
153103     PERFORM IMS-GU-WDGX6332                                              
153203     IF SEGMENT-FINNS                                                     
153303****LEVEL2*****                                                           
153403       MOVE +1 TO RAD-IX                                                  
153503       MOVE 6332-IDUSER(1) TO W-IDUSER1                                   
153603       MOVE 6332-IDUSER(2) TO W-IDUSER2                                   
153703       MOVE 6332-IDUSER(3) TO W-IDUSER3                                   
153803       MOVE 6332-IDUSER(4) TO W-IDUSER4                                   
153903       PERFORM FA-LAES-ARTS11                                             
154003       IF SEGMENT-FINNS                                                   
154103         IF PERS-WS-IDDC NOT = MOD-IDDC(RAD-IX)                           
154203           IF W-IDUSER1 = MSGI-IDUSER                                     
154303           OR W-IDUSER2 = MSGI-IDUSER                                     
154403           OR W-IDUSER3 = MSGI-IDUSER                                     
154503           OR W-IDUSER4 = MSGI-IDUSER                                     
154603             CONTINUE                                                     
154703           ELSE                                                           
154803             IF PERS-CDC                                                  
154903               MOVE MFS-CLOSE-FIELD TO                                    
155003                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
155103               MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                         
155203             ELSE                                                         
155303               MOVE MFS-CLOSE-FIELD TO                                    
155403                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
155503                               MOD-KDLEVSP-UPD-ATTR(RAD-IX)               
155603               MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                         
155703             END-IF                                                       
155803           END-IF                                                         
155903         END-IF                                                           
156003         ADD +1 TO RAD-IX                                                 
156103       END-IF                                                             
156203****LEVEL3*****                                                           
156303       PERFORM IMS-GNP-WDGX6334-2                                         
156403       PERFORM UNTIL RAD-IX > RAD-IX-MAX OR SEGMENT-SAKNAS                
156503         MOVE 6334-IDDC        TO WS-IDDC-TREE                            
156603         PERFORM FA-LAES-ARTS11                                           
156703         IF SEGMENT-FINNS                                                 
156803           ADD +1 TO RAD-IX                                               
156903         END-IF                                                           
157003         PERFORM IMS-GNP-WDGX6334-2                                       
157103       END-PERFORM                                                        
157203       PERFORM UNTIL RAD-IX > RAD-IX-MAX                                  
157303         MOVE MFS-STAENG-FAELT TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)            
157403                                 MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)        
157503                                  MOD-TEKVAL-ATTR(RAD-IX)                 
157603         MOVE SPACE            TO SPAR-IDDC-TAB(RAD-IX)                   
157703         ADD +1 TO RAD-IX                                                 
157803       END-PERFORM                                                        
157903     END-IF                                                               
158003     .                                                                    
158103     EJECT                                                                
158203                                                                          
158303 FBE-LAES-ARTS11-SHOWALL SECTION.                                         
158403                                                                          
158503*    --- Rensa först alla utdatafält                                      
158603     PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                                   
158703     PERFORM IMS-GET-UNIK-WLARTS01                                        
158803     MOVE +1 TO RAD-IX                                                    
158903     PERFORM UNTIL RAD-IX > RAD-IX-MAX OR SEGMENT-SAKNAS                  
159003         PERFORM FAB-LAES-ARTS11                                          
159103         ADD +1 TO RAD-IX                                                 
159203     END-PERFORM                                                          
159303     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
159403*      MOVE MFS-STAENG-FAELT TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)              
159503       MOVE MFS-STAENG-FAELT TO MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
159603                                MOD-TEKVAL-ATTR(RAD-IX)                   
159703*      MOVE SPACE            TO SPAR-IDDC-TAB(RAD-IX)                     
159803       ADD +1 TO RAD-IX                                                   
159903     END-PERFORM                                                          
160003     IF SEGMENT-FINNS                                                     
160103       MOVE ARTS-SLAG-IDDC    TO SPAR-IDDC2                               
160203       MOVE INF-MORE-INFO-EXISTS         TO MED-IDMFSINF                  
160303       CALL WMEDKONV USING MED-WMEDAREA                                   
160403       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
160503     ELSE                                                                 
160603       MOVE 'THIS IS LAST PAGE' TO MOD-TEMFSINF                           
160703       MOVE SPACE             TO SPAR-IDDC2                               
160803     END-IF                                                               
160903     .                                                                    
161003     EJECT                                                                
161103                                                                          
161203 FBF-LAES-ARTS11-TYPE    SECTION.                                         
161303                                                                          
161403*    --- Rensa först alla utdatafält                                      
161503     PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                                   
161603****LEVEL2*****                                                           
161703     PERFORM IMS-GU-WDGX6331                                              
161803     PERFORM IMS-GNP-WDGX6332-2                                           
161903     MOVE +1 TO RAD-IX                                                    
162003     PERFORM UNTIL RAD-IX > RAD-IX-MAX OR SEGMENT-SAKNAS                  
162103       IF SEGMENT-FINNS                                                   
162203         MOVE 6332-IDUSER(1) TO W-IDUSER1                                 
162303         MOVE 6332-IDUSER(2) TO W-IDUSER2                                 
162403         MOVE 6332-IDUSER(3) TO W-IDUSER3                                 
162503         MOVE 6332-IDUSER(4) TO W-IDUSER4                                 
162603         IF 6332-KDDC = 'N'                                               
162703           MOVE 6332-IDDC      TO WS-IDDC-TREE                            
162803           PERFORM FA-LAES-ARTS11                                         
162903           IF SEGMENT-FINNS                                               
163003             IF PERS-WS-IDDC NOT = MOD-IDDC(RAD-IX)                       
163103               IF W-IDUSER1 = MSGI-IDUSER                                 
163203               OR W-IDUSER2 = MSGI-IDUSER                                 
163303               OR W-IDUSER3 = MSGI-IDUSER                                 
163403               OR W-IDUSER4 = MSGI-IDUSER                                 
163503                 CONTINUE                                                 
163603               ELSE                                                       
163703                 IF PERS-CDC                                              
163803                   MOVE MFS-CLOSE-FIELD TO                                
163903                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
164003                   MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                     
164103                 ELSE                                                     
164203                   MOVE MFS-CLOSE-FIELD TO                                
164303                               MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)          
164403                               MOD-KDLEVSP-UPD-ATTR(RAD-IX)               
164503                   MOVE 'Y' TO SPAR-CLOSE-TAB(RAD-IX)                     
164603                 END-IF                                                   
164703               END-IF                                                     
164803             END-IF                                                       
164903             ADD +1 TO RAD-IX                                             
165003           END-IF                                                         
165103         END-IF                                                           
165203         PERFORM IMS-GNP-WDGX6332-2                                       
165303       END-IF                                                             
165403     END-PERFORM                                                          
165503     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
165603       MOVE MFS-STAENG-FAELT TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)              
165703                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
165803                                MOD-TEKVAL-ATTR(RAD-IX)                   
165903       MOVE SPACE            TO SPAR-IDDC-TAB(RAD-IX)                     
166003       ADD +1 TO RAD-IX                                                   
166103     END-PERFORM                                                          
166203     .                                                                    
166303     EJECT                                                                
166403                                                                          
166503 G-KOLLA-INDATA   SECTION.                                                
166603     SKIP2                                                                
166703     SET INDATA-OK TO TRUE                                                
166803     MOVE MSGI-IDDC TO PERS-WS-IDDC                                       
166903                                                                          
167003     IF MID-INPUT = ALL '+'                                               
167103       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
167203       CALL WMEDKONV USING MED-WMEDAREA                                   
167303       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
167403       PERFORM MFS-ROER-EJ-FAELT-UT                                       
167503       PERFORM MFS-ROER-EJ-FAELT-IN                                       
167603       SET INDATA-FEL TO TRUE                                             
167703     ELSE                                                                 
167803*      --- FORMELL KONTROLL                                               
167903       PERFORM GA-KOLLA-INDATAFAELT-FORMELLT                              
168003       IF INDATA-FEL                                                      
168103         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
168203         CALL WMEDKONV USING MED-WMEDAREA                                 
168303         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
168403         PERFORM MFS-ROER-EJ-FAELT-UT                                     
168503         PERFORM MFS-ROER-EJ-FAELT-IN                                     
168603       ELSE                                                               
168703         PERFORM IMS-GET-WLARTC01-TEST                                    
168803         IF SEGMENT-SAKNAS                                                
168903*          --- Artikeln finns INTE på ARTC                                
169003           MOVE ERR-MISSING-IN-REGISTER TO MED-IDMFSFEL                   
169103           CALL WMEDKONV USING MED-WMEDAREA                               
169203           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
169303           PERFORM MFS-RENSA-FAELT-IN-CDC                                 
169403           PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                             
169503           PERFORM MFS-RENSA-FAELT-UT                                     
169603           MOVE MFS-RENSA-FAELT TO MOD-TIMAIL-KVAL                        
169703           SET INDATA-FEL TO TRUE                                         
169803         ELSE                                                             
169903           PERFORM IMS-GET-NEXT-WLARTC11                                  
170003*          --- segment skall finnas !!!!!!!!!!!                           
170103           PERFORM GB-KOLLA-GLOBAL-O-CDC-INDATA                           
170203                                                                          
170303           PERFORM IMS-GET-UNIK-WLARTS01                                  
170403           IF SEGMENT-FINNS                                               
170503             MOVE +1 TO RAD-IX                                            
170603             PERFORM UNTIL SEGMENT-SAKNAS OR RAD-IX > RAD-IX-MAX          
170703*              PERFORM IMS-GET-NEXT-WLARTS11                              
170803               PERFORM GC-KOLLA-DC-INDATA                                 
170903               ADD +1 TO RAD-IX                                           
171003             END-PERFORM                                                  
171103           ELSE                                                           
171203*            --- segment BEHÖVER INTE FINNAS!!!!!!                        
171303             PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                           
171403             PERFORM MFS-RENSA-FAELT-UT-SDC-NDC                           
171503             PERFORM MFS-RENSA-FAELT-IN-UT-SDC-NDC                        
171603           END-IF                                                         
171703                                                                          
171803           IF MID-FLMAIL = 'Y' OR 'J'                                     
171903              IF MID-KDLEVSP-GLOB = 20                                    
172003              OR MID-KDLEVSP-DC11 = 21                                    
172103                 MOVE JA TO SW-MAIL                                       
172203              ELSE                                                        
172303                 MOVE NEJ TO INDATA-SW                                    
172403                             SW-MAIL                                      
172503              END-IF                                                      
172603           END-IF                                                         
172703                                                                          
172803           IF INDATA-FEL                                                  
172903             MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                  
173003             CALL WMEDKONV USING MED-WMEDAREA                             
173103             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
173203             PERFORM MFS-ROER-EJ-FAELT-UT                                 
173303             PERFORM MFS-ROER-EJ-FAELT-IN                                 
173403           END-IF                                                         
173503         END-IF                                                           
173603       END-IF                                                             
173703     END-IF                                                               
173803     .                                                                    
173903     EJECT                                                                
174003 GA-KOLLA-INDATAFAELT-FORMELLT SECTION.                                   
174103     SKIP2                                                                
174203     IF MID-TEARTNOT-GLOB NOT = ALL '+'                                   
174303       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-GLOB-ATTR                
174403     END-IF                                                               
174503*    ---  TEKVAL ---                                                      
174603     MOVE +1 TO RAD-IX                                                    
174703     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
174803       IF MID-TEKVAL(RAD-IX) NOT = ALL '+'                                
174903         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEKVAL-ATTR(RAD-IX)             
175003       END-IF                                                             
175103       ADD +1 TO RAD-IX                                                   
175203     END-PERFORM                                                          
175303*    ---  KDLEVSP ---                                                     
175403     IF MID-KDLEVSP-GLOB NOT = ALL '+'                                    
175503       IF MID-KDLEVSP-GLOB NOT NUMERIC                                    
175603         MOVE MFS-NUM-FAELT-FEL TO MOD-KDLEVSP-GLOB-UPD-ATTR              
175703         MOVE NEJ TO INDATA-SW                                            
175803       ELSE                                                               
175903         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDLEVSP-GLOB-UPD-ATTR            
176003       END-IF                                                             
176103     END-IF                                                               
176203     IF MID-KDLEVSP-DC11 NOT = ALL '+'                                    
176303       IF MID-KDLEVSP-DC11 NOT NUMERIC                                    
176403         MOVE MFS-NUM-FAELT-FEL TO MOD-KDLEVSP-DC11-UPD-ATTR              
176503         MOVE NEJ TO INDATA-SW                                            
176603       ELSE                                                               
176703         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDLEVSP-DC11-UPD-ATTR            
176803       END-IF                                                             
176903     END-IF                                                               
177003     MOVE +1 TO RAD-IX                                                    
177103     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
177203       IF MID-KDLEVSP(RAD-IX) NOT = ALL '+'                               
177303         IF MID-KDLEVSP(RAD-IX) NOT NUMERIC                               
177403           MOVE MFS-NUM-FAELT-FEL TO MOD-KDLEVSP-UPD-ATTR(RAD-IX)         
177503           MOVE NEJ TO INDATA-SW                                          
177603         ELSE                                                             
177703           MOVE MFS-NUM-FAELT-RAETT TO                                    
177803                             MOD-KDLEVSP-UPD-ATTR(RAD-IX)                 
177903         END-IF                                                           
178003       END-IF                                                             
178103       ADD +1 TO RAD-IX                                                   
178203     END-PERFORM                                                          
178303*    ---  KVSPARR-KVAL ---                                                
178403     IF MID-KVSPARR-KVAL-DC11 NOT = ALL '+'                               
178503       IF MID-KVSPARR-KVAL-DC11 NOT NUMERIC                               
178603         MOVE MFS-NUM-FAELT-FEL TO MOD-KVSPARR-KVAL-DC11-UPD-ATTR         
178703         MOVE NEJ TO INDATA-SW                                            
178803       ELSE                                                               
178903         MOVE MFS-NUM-FAELT-RAETT TO                                      
179003                                   MOD-KVSPARR-KVAL-DC11-UPD-ATTR         
179103       END-IF                                                             
179203     END-IF                                                               
179303     MOVE +1 TO RAD-IX                                                    
179403     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
179503       IF MID-KVSPARR-KVAL(RAD-IX) NOT = ALL '+'                          
179603         IF MID-KVSPARR-KVAL(RAD-IX) NOT NUMERIC                          
179703           MOVE MFS-NUM-FAELT-FEL TO                                      
179803                          MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)               
179903           MOVE NEJ TO INDATA-SW                                          
180003         ELSE                                                             
180103           MOVE MFS-NUM-FAELT-RAETT TO                                    
180203                          MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)               
180303         END-IF                                                           
180403       END-IF                                                             
180503       ADD +1 TO RAD-IX                                                   
180603     END-PERFORM                                                          
180703                                                                          
180803     IF MID-FLMAIL NOT = ALL '+'                                          
180903        IF MID-FLMAIL = 'Y' OR 'J' OR 'N'                                 
181003           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMAIL-ATTR                   
181103        ELSE                                                              
181203           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLMAIL-ATTR                     
181303           MOVE NEJ TO INDATA-SW                                          
181403        END-IF                                                            
181503     END-IF                                                               
181603     .                                                                    
181703     EJECT                                                                
181803                                                                          
181903 GB-KOLLA-GLOBAL-O-CDC-INDATA  SECTION.                                   
182003     SKIP2                                                                
182103*   GLOB & DC11  ---- TEARTNOT                                            
182203     IF MID-TEARTNOT-GLOB NOT = ALL '+'                                   
182303       IF (     MID-KDLEVSP-GLOB NOT = ALL '+'                            
182403            AND MID-KDLEVSP-GLOB > ZERO )                                 
182503       OR (     MID-KDLEVSP-DC11 NOT = ALL '+'                            
182603            AND MID-KDLEVSP-DC11 > ZERO )                                 
182703*        --- Not SKALL sättas vid Spärrkod > 0                            
182803         IF MID-TEARTNOT-GLOB = SPACE                                     
182903           SET INDATA-FEL TO TRUE                                         
183003           MOVE MFS-ALFA-FAELT-FEL TO MOD-TEARTNOT-GLOB-ATTR              
183103           MOVE MFS-RENSA-FAELT    TO MOD-TEARTNOT-GLOB                   
183203         END-IF                                                           
183303       ELSE                                                               
183403         MOVE MFS-RENSA-FAELT      TO MOD-TEARTNOT-GLOB                   
183503         IF MID-TEARTNOT-GLOB NOT = SPACE                                 
183603*          --- Not får INTE sättas vid Spärrkod = 0                       
183703           SET INDATA-FEL TO TRUE                                         
183803           MOVE MFS-ALFA-FAELT-FEL TO MOD-TEARTNOT-GLOB-ATTR              
183903           MOVE MFS-ROER-EJ-FAELT  TO MOD-TEARTNOT-GLOB                   
184003         END-IF                                                           
184103       END-IF                                                             
184203     ELSE                                                                 
184303       MOVE MFS-RENSA-FAELT    TO MOD-TEARTNOT-GLOB                       
184403     END-IF                                                               
184503* GLOB---KDLEVSP                                                          
184603     IF MID-KDLEVSP-GLOB NOT = ALL '+'                                    
184703       EVALUATE MID-KDLEVSP-GLOB                                          
184803         WHEN ZERO                                                        
184903*          --- Ngn vill släppa alla spärrar till alla DC utom             
185003*          --- de som ev. har fått ny KOD/KVANT i denna inmatn            
185103             CONTINUE                                                     
185203         WHEN 20                                                          
185303*          --- CDC-personal vill spärra alla DC.                          
185403*          --- TEARTNOT måste vara ifylld.                                
185503           IF MID-TEARTNOT-GLOB = SPACE OR ALL '+'                        
185603             SET INDATA-FEL TO TRUE                                       
185703             MOVE MFS-ALFA-FAELT-FEL TO MOD-TEARTNOT-GLOB-ATTR            
185803             MOVE MFS-RENSA-FAELT  TO MOD-TEARTNOT-GLOB                   
185903           END-IF                                                         
186003                                                                          
186103         WHEN OTHER                                                       
186203           SET INDATA-FEL TO TRUE                                         
186303           MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-GLOB-UPD                 
186403           MOVE MFS-NUM-FAELT-FEL TO MOD-KDLEVSP-GLOB-UPD-ATTR            
186503       END-EVALUATE                                                       
186603     ELSE                                                                 
186703       MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-GLOB-UPD                       
186803     END-IF                                                               
186903                                                                          
187003* DC11---KDLEVSP                                                          
187103                                                                          
187203     IF MID-KDLEVSP-DC11 NOT = ALL '+'                                    
187303       EVALUATE MID-KDLEVSP-DC11                                          
187403         WHEN ZERO                                                        
187503*          --- Ngn vill släppa spärren på CDC                             
187603*          --- Kolla att ingen global spärr finns                         
187703           IF ARTC-CLAG-KDLEVSP = 20                                      
187803*            --- Global spärr måste släppas först                         
187903             IF MID-KDLEVSP-GLOB = ZERO                                   
188003*              --- OK,spärren släpps globalt i samma uppdatering          
188103               CONTINUE                                                   
188203             ELSE                                                         
188303               SET INDATA-FEL TO TRUE                                     
188403               MOVE MFS-NUM-FAELT-FEL TO MOD-KDLEVSP-DC11-UPD-ATTR        
188503               MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-DC11-UPD             
188603             END-IF                                                       
188703           ELSE                                                           
188803*            --- OK, ingen global spärr                                   
188903*            --- Övriga DC:n nollas oxå om de har kod 21                  
189003             CONTINUE                                                     
189103           END-IF                                                         
189203                                                                          
189303         WHEN 21                                                          
189403*          --- Kolla att ingen kvantitet finns uppdaterad                 
189503           IF ARTC-CLAG-KDLEVSP = 20                                      
189603*            --- Global spärr måste INTE släppas först                    
189703             IF MID-KDLEVSP-GLOB = ZERO                                   
189803*              --- Om man angivit 0 på global, sätts 21 endast på         
189903*              --- CDC och de andra nollas om de inte fått annat.         
190003*              --- OK,spärren släpps globalt i samma uppdatering          
190103               CONTINUE                                                   
190203             ELSE                                                         
190303*              --- Om man INTE rör global, vill man byta                  
190403*              --- till 21 på alla DC, som inte har fått annat.           
190503               CONTINUE                                                   
190603             END-IF                                                       
190703           END-IF                                                         
190803*          --- TEARTNOT måste OXÅ vara ifylld.                            
190903           IF MID-TEARTNOT-GLOB = SPACE OR ALL '+'                        
191003             SET INDATA-FEL TO TRUE                                       
191103             MOVE MFS-ALFA-FAELT-FEL TO MOD-TEARTNOT-GLOB-ATTR            
191203             MOVE MFS-RENSA-FAELT    TO MOD-TEARTNOT-GLOB                 
191303           END-IF                                                         
191403*          --- Kolla att ingen KVANT är registrerad                       
191503           IF ARTC-CLAG-KVSPARR-KVAL > ZERO                               
191603             SET INDATA-FEL TO TRUE                                       
191703             MOVE MFS-NUM-FAELT-FEL TO MOD-KDLEVSP-DC11-UPD-ATTR          
191803             MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-DC11-UPD               
191903           ELSE                                                           
192003             CONTINUE                                                     
192103*            --- Kod sätts på övriga DC:n utan registrerad KVANT          
192203*            --- i H-UPPDAT.. sektionen.                                  
192303           END-IF                                                         
192403                                                                          
192503         WHEN OTHER                                                       
192603           SET INDATA-FEL TO TRUE                                         
192703           MOVE MFS-NUM-FAELT-FEL TO MOD-KDLEVSP-DC11-UPD-ATTR            
192803           MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-DC11-UPD                 
192903       END-EVALUATE                                                       
193003     ELSE                                                                 
193103       MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-DC11-UPD                       
193203     END-IF                                                               
193303                                                                          
193403* DC11--- KVSPARR-KVAL                                                    
193503                                                                          
193603     IF MID-KVSPARR-KVAL-DC11 NOT = ALL '+'                               
193703*      --- Kolla att ingen spärrkod finns registrerad                     
193803       IF MID-KVSPARR-KVAL-DC11 = ZERO                                    
193903*        --- OK, släpper antalsmässig kvalitétsspärr                      
194003         CONTINUE                                                         
194103       ELSE                                                               
194203*        --- Kolla att spärrkod ÄR NOLL eller BLIR NOLL                   
194303         IF ARTC-CLAG-KDLEVSP > ZERO                                      
194403           IF MID-KDLEVSP-DC11 = ALL '+'                                  
194503             IF MID-KDLEVSP-GLOB = ALL '+'                                
194603               SET INDATA-FEL TO TRUE                                     
194703               MOVE MFS-NUM-FAELT-FEL TO                                  
194803                                    MOD-KVSPARR-KVAL-DC11-UPD-ATTR        
194903               MOVE MFS-ROER-EJ-FAELT TO MOD-KVSPARR-KVAL-DC11-UPD        
195003             ELSE                                                         
195103               IF MID-KDLEVSP-GLOB NOT = ZERO                             
195203                 SET INDATA-FEL TO TRUE                                   
195303                 MOVE MFS-NUM-FAELT-FEL TO                                
195403                                    MOD-KVSPARR-KVAL-DC11-UPD-ATTR        
195503                 MOVE MFS-ROER-EJ-FAELT TO                                
195603                                         MOD-KVSPARR-KVAL-DC11-UPD        
195703               END-IF                                                     
195803             END-IF                                                       
195903           ELSE                                                           
196003             IF MID-KDLEVSP-DC11 NOT = ZERO                               
196103               SET INDATA-FEL TO TRUE                                     
196203               MOVE MFS-NUM-FAELT-FEL TO                                  
196303                                 MOD-KVSPARR-KVAL-DC11-UPD-ATTR           
196403               MOVE MFS-ROER-EJ-FAELT TO MOD-KVSPARR-KVAL-DC11            
196503             END-IF                                                       
196603           END-IF                                                         
196703         ELSE                                                             
196803*          --- Ingen spärr på basen                                       
196903           IF ( MID-KDLEVSP-GLOB NOT = ALL '+'                            
197003                AND MID-KDLEVSP-GLOB > ZERO )                             
197103           OR ( MID-KDLEVSP-DC11 NOT = ALL '+'                            
197203                AND MID-KDLEVSP-DC11 > ZERO )                             
197303               SET INDATA-FEL TO TRUE                                     
197403               MOVE MFS-NUM-FAELT-FEL TO                                  
197503                                    MOD-KVSPARR-KVAL-DC11-UPD-ATTR        
197603               MOVE MFS-ROER-EJ-FAELT TO MOD-KVSPARR-KVAL-DC11-UPD        
197703           END-IF                                                         
197803         END-IF                                                           
197903       END-IF                                                             
198003     ELSE                                                                 
198103       MOVE MFS-RENSA-FAELT TO MOD-KVSPARR-KVAL-DC11                      
198203     END-IF                                                               
198303     .                                                                    
198403     EJECT                                                                
198503                                                                          
198603 GC-KOLLA-DC-INDATA  SECTION.                                             
198703     SKIP2                                                                
198803*        KDLEVSP får bara sättas till 21 av CDC-användare.                
198903*        eller NOLL om den förut var 21.                                  
199003*        CDC-användare får också sätta kod 22                             
199103*        eller NOLL om den förut var 22.                                  
199203*        ANTALS-SPÄRR får sättas även om det finns GLOBALSPÄRR 20         
199303     MOVE SPAR-IDDC-TAB(RAD-IX) TO W-IDDC                                 
199403     PERFORM GCA-KOLLA-LEVEL                                              
199503     PERFORM IMS-GET-UNIK-WLARTS11                                        
199603     IF SEGMENT-FINNS                                                     
199703* DC--- KDLEVSP                                                           
199803       IF MID-KDLEVSP(RAD-IX) NOT = ALL '+'                               
199903         EVALUATE MID-KDLEVSP(RAD-IX)                                     
200003           WHEN ZERO                                                      
200103*            --- Ngn vill släppa spärren på DC                            
200203*            --- Kolla att ingen global spärr finns                       
200303             IF PERS-CDC                                                  
200403               IF ARTS-SLAG-KDLEVSP = 20 OR 21 OR 22                      
200503*                --- Får nollas av CDC-folk                               
200603                 IF ARTS-SLAG-KDLEVSP = 20                                
200703                   IF MID-KDLEVSP-GLOB = ZERO                             
200803                     CONTINUE                                             
200903                   ELSE                                                   
201003*                    --- KOD 20 måste nollas på globalnivå                
201103                     SET INDATA-FEL TO TRUE                               
201203                     MOVE MFS-NUM-FAELT-FEL TO                            
201303                                MOD-KDLEVSP-UPD-ATTR(RAD-IX)              
201403                     MOVE MFS-ROER-EJ-FAELT TO                            
201503                                MOD-KDLEVSP-UPD(RAD-IX)                   
201603                   END-IF                                                 
201703                 END-IF                                                   
201803               ELSE                                                       
201903                 IF ARTS-SLAG-KDLEVSP > 21                                
202003                   SET INDATA-FEL TO TRUE                                 
202103                   MOVE MFS-NUM-FAELT-FEL TO                              
202203                                MOD-KDLEVSP-UPD-ATTR(RAD-IX)              
202303                   MOVE MFS-ROER-EJ-FAELT TO                              
202403                                MOD-KDLEVSP-UPD(RAD-IX)                   
202503                 END-IF                                                   
202603               END-IF                                                     
202703             ELSE                                                         
202803               IF ARTS-SLAG-KDLEVSP = 20 OR 21                            
202903                 SET INDATA-FEL TO TRUE                                   
203003                 MOVE MFS-NUM-FAELT-FEL TO                                
203103                                  MOD-KDLEVSP-UPD-ATTR(RAD-IX)            
203203                 MOVE MFS-ROER-EJ-FAELT TO                                
203303                                  MOD-KDLEVSP-UPD(RAD-IX)                 
203403               END-IF                                                     
203503             END-IF                                                       
203603                                                                          
203703           WHEN 21                                                        
203803*            --- Kolla att ingen global spärr finns                       
203903             IF PERS-CDC                                                  
204003               IF ARTS-SLAG-KDLEVSP = 20                                  
204103                 IF MID-KDLEVSP-GLOB = ZERO                               
204203*                  --- OK,spärren släpps globalt i samma uppdat           
204303*                  --- Kolla att ingen kvantitet finns uppdaterad         
204403                   IF ARTS-SLAG-KVSPARR-KVAL > ZERO                       
204503                     IF MID-KVSPARR-KVAL(RAD-IX) = ZERO                   
204603                       CONTINUE                                           
204703                     ELSE                                                 
204803                       SET INDATA-FEL TO TRUE                             
204903                       MOVE MFS-NUM-FAELT-FEL TO                          
205003                                    MOD-KDLEVSP-UPD-ATTR(RAD-IX)          
205103                       MOVE MFS-ROER-EJ-FAELT TO                          
205203                                    MOD-KDLEVSP-UPD(RAD-IX)               
205303                     END-IF                                               
205403                   END-IF                                                 
205503                 ELSE                                                     
205603                   SET INDATA-FEL TO TRUE                                 
205703                   MOVE MFS-NUM-FAELT-FEL TO                              
205803                                    MOD-KDLEVSP-UPD-ATTR(RAD-IX)          
205903                   MOVE MFS-ROER-EJ-FAELT TO                              
206003                                    MOD-KDLEVSP-UPD(RAD-IX)               
206103                 END-IF                                                   
206203               ELSE                                                       
206303*                --- OK men kolla antal-kvalitet                          
206403                 IF ARTS-SLAG-KVSPARR-KVAL > ZERO                         
206503                   IF MID-KVSPARR-KVAL(RAD-IX) = ZERO                     
206603                     CONTINUE                                             
206703                   ELSE                                                   
206803*                    --- KVSPARR-KVAL får bara finnas med KOD 20          
206903                     SET INDATA-FEL TO TRUE                               
207003                     MOVE MFS-NUM-FAELT-FEL TO                            
207103                                  MOD-KDLEVSP-UPD-ATTR(RAD-IX)            
207203                     MOVE MFS-ROER-EJ-FAELT TO                            
207303                                  MOD-KDLEVSP-UPD(RAD-IX)                 
207403                   END-IF                                                 
207503                 END-IF                                                   
207603               END-IF                                                     
207703** SKA GCB-KOLL VARA MED                                                  
207803*              IF WS-LEVEL = 2                                            
207903*                PERFORM GCB-KOLLA-LEVEL3                                 
208003*              END-IF                                                     
208103             ELSE                                                         
208203               SET INDATA-FEL TO TRUE                                     
208303               MOVE MFS-NUM-FAELT-FEL TO                                  
208403                                 MOD-KDLEVSP-UPD-ATTR(RAD-IX)             
208503               MOVE MFS-ROER-EJ-FAELT TO                                  
208603                                 MOD-KDLEVSP-UPD(RAD-IX)                  
208703             END-IF                                                       
208803                                                                          
208903           WHEN 22                                                        
209003*            --- Kolla att ingen global spärr finns                       
209103             IF  PERS-CDC                                                 
209203             OR ((WS-LEVEL = 2) AND                                       
209303                 ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR              
209403                 (MSGI-IDUSER = W-IDUSER1) OR                             
209503                 (MSGI-IDUSER = W-IDUSER2) OR                             
209603                 (MSGI-IDUSER = W-IDUSER3) OR                             
209703                 (MSGI-IDUSER = W-IDUSER4)))                              
209803             OR ((WS-LEVEL = 3) AND                                       
209903                 ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR              
210003                 (PERS-WS-IDDC = 6332-KEY-FB-AREA-IDDC)   OR              
210103                 (MSGI-IDUSER = W-IDUSER1) OR                             
210203                 (MSGI-IDUSER = W-IDUSER2) OR                             
210303                 (MSGI-IDUSER = W-IDUSER3) OR                             
210403                 (MSGI-IDUSER = W-IDUSER4)))                              
210503               IF ARTS-SLAG-KDLEVSP = 20                                  
210603                 SET INDATA-FEL TO TRUE                                   
210703                 MOVE MFS-NUM-FAELT-FEL TO                                
210803                                 MOD-KDLEVSP-UPD-ATTR(RAD-IX)             
210903                 MOVE MFS-ROER-EJ-FAELT TO                                
211003                                 MOD-KDLEVSP-UPD(RAD-IX)                  
211103               ELSE                                                       
211203                 IF ARTS-SLAG-KDLEVSP = 21                                
211303                 AND (((WS-LEVEL = 2) AND                                 
211403                     ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR          
211503                     (MSGI-IDUSER = W-IDUSER1) OR                         
211603                     (MSGI-IDUSER = W-IDUSER2) OR                         
211703                     (MSGI-IDUSER = W-IDUSER3) OR                         
211803                     (MSGI-IDUSER = W-IDUSER4)))                          
211903                 OR ((WS-LEVEL = 3) AND                                   
212003                     ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR          
212103                     (PERS-WS-IDDC = 6332-KEY-FB-AREA-IDDC) OR            
212203                     (MSGI-IDUSER = W-IDUSER1) OR                         
212303                     (MSGI-IDUSER = W-IDUSER2) OR                         
212403                     (MSGI-IDUSER = W-IDUSER3) OR                         
212503                     (MSGI-IDUSER = W-IDUSER4))))                         
212603*                  --- NDC får inte overrida en 21-kod                    
212703                   SET INDATA-FEL TO TRUE                                 
212803                   MOVE MFS-NUM-FAELT-FEL TO                              
212903                                 MOD-KDLEVSP-UPD-ATTR(RAD-IX)             
213003                   MOVE MFS-ROER-EJ-FAELT TO                              
213103                                 MOD-KDLEVSP-UPD(RAD-IX)                  
213203                 END-IF                                                   
213303               END-IF                                                     
213403** SKA GCB-KOLL VARA MED                                                  
213503*              IF WS-LEVEL = 2                                            
213603*                PERFORM GCB-KOLLA-LEVEL3                                 
213703*              END-IF                                                     
213803             ELSE                                                         
213903               SET INDATA-FEL TO TRUE                                     
214003               MOVE MFS-NUM-FAELT-FEL TO                                  
214103                                MOD-KDLEVSP-UPD-ATTR(RAD-IX)              
214203               MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-UPD(RAD-IX)          
214303             END-IF                                                       
214403                                                                          
214503           WHEN OTHER                                                     
214603             SET INDATA-FEL TO TRUE                                       
214703             MOVE MFS-NUM-FAELT-FEL TO                                    
214803                              MOD-KDLEVSP-UPD-ATTR(RAD-IX)                
214903             MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-UPD(RAD-IX)            
215003         END-EVALUATE                                                     
215103       ELSE                                                               
215203         MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-UPD(RAD-IX)                  
215303       END-IF                                                             
215403* DC--- KVSPARR-KVAL                                                      
215503       IF MID-KVSPARR-KVAL(RAD-IX) NOT = ALL '+'                          
215603         IF PERS-CDC                                                      
215703           MOVE MFS-NUM-FAELT-FEL TO                                      
215803                            MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)             
215903           MOVE MFS-ROER-EJ-FAELT TO MOD-KVSPARR-KVAL-UPD(RAD-IX)         
216003         ELSE                                                             
216103           IF MID-KVSPARR-KVAL(RAD-IX) = ZERO                             
216203*            --- OK, släpper antalsmässig kvalitétsspärr                  
216303             CONTINUE                                                     
216403           ELSE                                                           
216503*            --- DC-personal vill spärra antal..                          
216603             IF MID-KDLEVSP(RAD-IX) NOT = ALL '+'                         
216703               IF ARTS-SLAG-KDLEVSP = 22                                  
216803                 IF MID-KDLEVSP(RAD-IX) = ZERO                            
216903                   CONTINUE                                               
217003                 ELSE                                                     
217103                   SET INDATA-FEL TO TRUE                                 
217203                   MOVE MFS-NUM-FAELT-FEL TO                              
217303                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
217403                   MOVE MFS-ROER-EJ-FAELT TO                              
217503                                MOD-KVSPARR-KVAL-UPD(RAD-IX)              
217603                 END-IF                                                   
217703               ELSE                                                       
217803                 IF ARTS-SLAG-KDLEVSP = 21                                
217903*                  ---  CDC-spärrat                                       
218003                   SET INDATA-FEL TO TRUE                                 
218103                   MOVE MFS-NUM-FAELT-FEL TO                              
218203                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
218303                   MOVE MFS-ROER-EJ-FAELT TO                              
218403                                MOD-KVSPARR-KVAL-UPD(RAD-IX)              
218503                 END-IF                                                   
218603               END-IF                                                     
218703             ELSE                                                         
218803               IF ARTS-SLAG-KDLEVSP = 20                                  
218903*                --- Skall vara möjligt enl. Sussi och Per 970418         
219003                 CONTINUE                                                 
219103               ELSE                                                       
219203                 IF ARTS-SLAG-KDLEVSP NOT = ZERO                          
219303*                  --- Annan KOD finns, abbans !                          
219403                   SET INDATA-FEL TO TRUE                                 
219503                   MOVE MFS-NUM-FAELT-FEL TO                              
219603                                MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)         
219703                   MOVE MFS-ROER-EJ-FAELT TO                              
219803                                MOD-KVSPARR-KVAL-UPD(RAD-IX)              
219903                 END-IF                                                   
220003               END-IF                                                     
220103             END-IF                                                       
220203           END-IF                                                         
220303         END-IF                                                           
220403       ELSE                                                               
220503         MOVE MFS-RENSA-FAELT TO MOD-KVSPARR-KVAL-UPD(RAD-IX)             
220603       END-IF                                                             
220703* DC--- TEKVAL                                                            
220803       IF MID-TEKVAL(RAD-IX) NOT = ALL '+'                                
220903         IF ARTS-SLAG-KDLEVSP >= 20                                       
221003         OR ARTS-SLAG-KVSPARR-KVAL >= ZERO                                
221103           IF PERS-CDC                                                    
221203           OR ((WS-LEVEL = 2) AND                                         
221303               ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR                
221403               (MSGI-IDUSER = W-IDUSER1) OR                               
221503               (MSGI-IDUSER = W-IDUSER2) OR                               
221603               (MSGI-IDUSER = W-IDUSER3) OR                               
221703               (MSGI-IDUSER = W-IDUSER4)))                                
221803           OR ((WS-LEVEL = 3)                           AND               
221903               ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR                
222003               (PERS-WS-IDDC = 6332-KEY-FB-AREA-IDDC)   OR                
222103               (MSGI-IDUSER = W-IDUSER1) OR                               
222203               (MSGI-IDUSER = W-IDUSER2) OR                               
222303               (MSGI-IDUSER = W-IDUSER3) OR                               
222403               (MSGI-IDUSER = W-IDUSER4)))                                
222503             CONTINUE                                                     
222603           ELSE                                                           
222703             MOVE MFS-RENSA-FAELT  TO MOD-TEKVAL(RAD-IX)                  
222803             MOVE MFS-FORMATETS-ATTR TO MOD-TEKVAL-ATTR(RAD-IX)           
222903           END-IF                                                         
223003         ELSE                                                             
223103           MOVE MFS-RENSA-FAELT    TO MOD-TEKVAL(RAD-IX)                  
223203           MOVE MFS-FORMATETS-ATTR TO MOD-TEKVAL-ATTR(RAD-IX)             
223303         END-IF                                                           
223403       ELSE                                                               
223503         MOVE MFS-RENSA-FAELT    TO MOD-TEKVAL(RAD-IX)                    
223603       END-IF                                                             
223703     ELSE                                                                 
223803       MOVE MFS-RENSA-FAELT TO MOD-KDLEVSP-UPD(RAD-IX)                    
223903                               MOD-KVSPARR-KVAL-UPD(RAD-IX)               
224003                               MOD-TEKVAL(RAD-IX)                         
224103*      --- Inget WDK711-segment                                           
224203     END-IF                                                               
224303     .                                                                    
224403     EJECT                                                                
224503 GCA-KOLLA-LEVEL SECTION.                                                 
224603                                                                          
224703     PERFORM IMS-GU-WDGX6331                                              
224803     IF SEGMENT-FINNS                                                     
224903       IF W-IDDC = '11'                                                   
225003         MOVE +1 TO WS-LEVEL                                              
225103       ELSE                                                               
225203         MOVE W-IDDC  TO W-6332-IDDC                                      
225303         PERFORM IMS-GNP-WDGX6332                                         
225403         IF SEGMENT-FINNS                                                 
225503           MOVE +2 TO WS-LEVEL                                            
225603           MOVE 6332-IDUSER(1) TO W-IDUSER1                               
225703           MOVE 6332-IDUSER(2) TO W-IDUSER2                               
225803           MOVE 6332-IDUSER(3) TO W-IDUSER3                               
225903           MOVE 6332-IDUSER(4) TO W-IDUSER4                               
226003         ELSE                                                             
226103           MOVE W-IDDC TO W-6334-IDDC                                     
226203           PERFORM IMS-GU-WDGX6331                                        
226303           PERFORM IMS-GNP-WDGX6334-2                                     
226403           IF SEGMENT-FINNS                                               
226503             MOVE +3 TO WS-LEVEL                                          
226603           END-IF                                                         
226703         END-IF                                                           
226803       END-IF                                                             
226903     ELSE                                                                 
227003       MOVE ZERO TO WS-LEVEL                                              
227103     END-IF                                                               
227203     .                                                                    
227303     EJECT                                                                
227403 GCB-KOLLA-LEVEL3 SECTION.                                                
227503                                                                          
227603     MOVE ARTS-SLAG-IDDC    TO W-6332-IDDC                                
227703     PERFORM IMS-GU-WDGX6332                                              
227803     IF SEGMENT-FINNS                                                     
227903       PERFORM IMS-GNP-WDGX6334                                           
228003       PERFORM UNTIL SEGMENT-SAKNAS                                       
228103         MOVE 6334-IDDC   TO W-IDDC-K7                                    
228203         PERFORM IMS-GU-WDK711                                            
228303         IF SEGMENT-FINNS                                                 
228403           IF WDK7-SLAG-KVSPARR-KVAL > ZERO                               
228503             SET INDATA-FEL TO TRUE                                       
228603             MOVE 'QUANT. BLOCKED ON LEVEL3 DC'                           
228703                  TO MOD-TEMFSINF                                         
228803           END-IF                                                         
228903           IF WDK7-SLAG-KDLEVSP = 21                                      
229003           AND MID-KDLEVSP(RAD-IX) = 22                                   
229103             SET INDATA-FEL TO TRUE                                       
229203             MOVE 'DELIVERY BLOCK ON LEVEL3 DC'                           
229303                  TO MOD-TEMFSINF                                         
229403           END-IF                                                         
229503         END-IF                                                           
229603         PERFORM IMS-GNP-WDGX6334                                         
229703       END-PERFORM                                                        
229803     END-IF                                                               
229903     .                                                                    
230003     EJECT                                                                
230103 K-UPPDATERA      SECTION.                                                
230203     SKIP2                                                                
230303     SET INDATA-OK TO TRUE                                                
230403     IF PERS-NDC-CN OR PERS-LDC-CN                                        
230503       PERFORM IMS-GET-UNIK-WLARTS01                                      
230603       IF SEGMENT-FINNS                                                   
230703         MOVE +1 TO RAD-IX                                                
230803         PERFORM UNTIL RAD-IX > RAD-IX-MAX OR INDATA-FEL                  
230903           MOVE SPAR-IDDC-TAB (RAD-IX) TO BLK-WS-IDDC                     
231003           IF BLK-NDC-CN OR BLK-LDC-CN                                    
231103             MOVE SPAR-IDDC-TAB (RAD-IX) TO W-IDDC                        
231203             PERFORM IMS-GET-HOLD-UNIK-WLARTS11                           
231303             IF ARTS-SLAG-KDLEVSP = 21 OR 0                               
231403               IF MID-KDLEVSP (RAD-IX) = 21 OR 0                          
231503                 MOVE MID-KDLEVSP (RAD-IX)                                
231603                                      TO ARTS-SLAG-KDLEVSP                
231703                 PERFORM IMS-REPL-WLARTS11                                
231704                 PERFORM HFB-CHECK-REPL-WGX2404                           
231705                                                                          
231803                 PERFORM GCA-KOLLA-LEVEL                                  
231903                 IF WS-LEVEL = 2                                          
232003                   IF WS-SHOW  = 'N'                                      
232103                     MOVE MID-KDLEVSP(RAD-IX)                             
232203                                        TO TEST-SLAG-KDLEVSP              
232303                     PERFORM HFA-UPD-LEVEL3-DC                            
232403                   END-IF                                                 
232503                 END-IF                                                   
232603               ELSE                                                       
232703                 IF MID-KDLEVSP (RAD-IX) NOT = ALL '+'                    
232803                   MOVE MFS-NUM-FAELT-FEL TO                              
232903                                  MOD-KDLEVSP-UPD-ATTR(Rad-ix)            
233003                   MOVE NEJ TO INDATA-SW                                  
233103                 END-IF                                                   
233203               END-IF                                                     
233303             END-IF                                                       
233403           END-IF                                                         
233503           ADD +1 TO RAD-IX                                               
233603         END-PERFORM                                                      
233703       END-IF                                                             
233803     END-IF                                                               
233903                                                                          
234003     IF INDATA-OK                                                         
234103       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
234203       CALL WMEDKONV USING MED-WMEDAREA                                   
234303       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
234403       PERFORM MFS-RENSA-FAELT-IN-CDC                                     
234503       PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                                 
234603     ELSE                                                                 
234703       MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                        
234803       CALL WMEDKONV USING MED-WMEDAREA                                   
234903       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
235003       PERFORM MFS-ROER-EJ-FAELT-UT                                       
235103       PERFORM MFS-ROER-EJ-FAELT-IN                                       
235203     END-IF                                                               
235303     .                                                                    
235403     EJECT                                                                
235503 H-UPPDATERA      SECTION.                                                
235603     SKIP2                                                                
235703     IF MID-KDLEVSP-GLOB NOT = ALL '+'                                    
235803     OR MID-KDLEVSP-DC11 NOT = ALL '+'                                    
235903     OR MID-KVSPARR-KVAL-DC11 NOT = ALL '+'                               
236003       PERFORM HE-UPD-KOD20-KODDC11                                       
236103     ELSE                                                                 
236203       PERFORM HF-UPD-SDC                                                 
236303     END-IF                                                               
236403                                                                          
236503     IF SW-MAIL = JA                                                      
236603        PERFORM HD-FIXA-MAIL                                              
236703     ELSE                                                                 
236803        MOVE MFS-RENSA-FAELT TO MOD-FLMAIL                                
236903                                MOD-TIMAIL-KVAL                           
237003     END-IF                                                               
237103                                                                          
237203     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
237303     CALL WMEDKONV USING MED-WMEDAREA                                     
237403     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
237503     PERFORM MFS-RENSA-FAELT-IN-CDC                                       
237603     PERFORM MFS-RENSA-FAELT-IN-SDC-NDC                                   
237703     .                                                                    
237803     EJECT                                                                
237903 HA-TAECKNING-FOR-CDC   SECTION.                                          
238003     SKIP2                                                                
238103*    --- DENNA FUNKTION flyttad från W6021100                             
238203*      FYSISK NYCKEL PÅ ROTEN: WDGXKEY                                    
238303*      (IDHTYP + IDDC + LOWVALUE)                                         
238403                                                                          
238503     IF ARTC-CLAG-KVROS > ZERO                                            
238603*                              ARTIKLAR KLARA FÖR TÄCKNING                
238703*                              RESTORDERREGISTER (PASSIVT)                
238803*                              FYSISK NYCKEL PÅ SEGMENTET: IDARTNR        
238903       MOVE MSGI-IDDC TO W-IDDC-4505                                      
239003                                                                          
239103       MOVE W-IDARTNR TO 4506-IDARTNR                                     
239203       MOVE 12        TO 4506-KDTAKORS                                    
239303       MOVE ZERO      TO 4506-KVANTMOT                                    
239403       PERFORM IMS-ISRT-4505-WL450511                                     
239503     END-IF                                                               
239603     .                                                                    
239703     EJECT                                                                
239803                                                                          
239903 HB-TAECKNING-FOR-NDC  SECTION.                                           
240003     SKIP2                                                                
240103*      FYSISK NYCKEL PÅ ROTEN: WDGXKEY                                    
240203*      (IDHTYP + IDDC + LOWVALUE)                                         
240303                                                                          
240403     IF ARTS-SLAG-KVROS-BULK > ZERO                                       
240503     OR ARTS-SLAG-KVROS-DAG  > ZERO                                       
240603*                              ARTIKLAR KLARA FÖR TÄCKNING                
240703*                              RESTORDERREGISTER (PASSIVT)                
240803*                              FYSISK NYCKEL PÅ SEGMENTET: IDARTNR        
240903       MOVE MSGI-IDDC TO W-IDDC-4505                                      
241003                                                                          
241103       MOVE W-IDARTNR TO 4506-IDARTNR                                     
241203       MOVE 12        TO 4506-KDTAKORS                                    
241303       MOVE ZERO      TO 4506-KVANTMOT                                    
241403       PERFORM IMS-ISRT-4505-WL450511                                     
241503     END-IF                                                               
241603     .                                                                    
241703     EJECT                                                                
241803                                                                          
241903 HD-FIXA-MAIL SECTION.                                                    
242003                                                                          
242103     PERFORM IMS-GET-WLARTC01-TEST                                        
242203     IF SEGMENT-FINNS                                                     
242303        MOVE ARTC-ART-IDLEVNR  TO SPAR-IDLEVNR                            
242403        MOVE ARTC-ART-IDFKNGRP TO SPAR-IDFKNGRP                           
242503        PERFORM IMS-GET-NEXT-WLARTC11                                     
242603        IF SEGMENT-FINNS                                                  
242703           MOVE ARTC-CLAG-IDBERED TO SPAR-IDBERED                         
242803           MOVE ARTC-CLAG-IDANSK  TO SPAR-IDANSK                          
242903*****                                                                     
243003           IF ARTC-CLAG-IDINK (1:3) NUMERIC                               
243103              MOVE ARTC-CLAG-IDINK (1:3) TO SPAR-idink                    
243203           ELSE                                                           
243303              IF ARTC-CLAG-IDINK (2:3) NUMERIC                            
243403                 MOVE ARTC-CLAG-IDINK (2:3) TO SPAR-idink                 
243503              ELSE                                                        
243603                 MOVE ZERO TO SPAR-idink                                  
243703              END-IF                                                      
243803           END-IF                                                         
243903*****                                                                     
244003                                                                          
244103           PERFORM HDA-LAS-WDP3                                           
244203                                                                          
244303           MOVE DAGENS-DATUM    TO MOD-TIMAIL-KVAL                        
244403           MOVE MFS-RENSA-FAELT TO MOD-FLMAIL                             
244503                                                                          
244603           MOVE W-IDARTNR     TO URV-IDARTNR                              
244703           MOVE SPAR-IDLEVNR  TO URV-IDLEVNR                              
244803           MOVE SPAR-IDMAIL1  TO URV-IDMAIL1                              
244903           MOVE SPAR-IDMAIL2  TO URV-IDMAIL2                              
245003           MOVE SPAR-IDMAIL3  TO URV-IDMAIL3                              
245103           MOVE SPAR-IDMAIL4  TO URV-IDMAIL4                              
245203           MOVE SPAR-IDPERSON TO URV-IDPERSON                             
245303           MOVE '6308'        TO MSGSOP-IDTRANS                           
245403           MOVE '1'           TO MSGSOP-KDMFSFOR                          
245503           MOVE 'W614S2'      TO MSGSOP-IDPROCESS                         
245603           MOVE 'O'           TO MSGSOP-KDSOPFUNK                         
245703                                                                          
245803           STRING 'URVAL(' WS-URVAL ') IDMAIL1('                          
245903                WS-IDMAIL1 ') IDMAIL2(' WS-IDMAIL2 ') IDMAIL3('           
246003                WS-IDMAIL3 ') IDMAIL4(' WS-IDMAIL4 ')'                    
246103                DELIMITED BY SIZE INTO MSGSOP-TESYMBV                     
246203                                                                          
246303           PERFORM IMS-INSERT-ALTMSG                                      
246403           MOVE INF-MAIL-SENT TO MOD-TEMFSFEL                             
246503        END-IF                                                            
246603     END-IF                                                               
246703     .                                                                    
246803     EJECT                                                                
246903 HDA-LAS-WDP3 SECTION.                                                    
247003                                                                          
247103     IF SPAR-IDANSK > +0                                                  
247203        MOVE 'ANSK    '  TO W-KDARBTYP                                    
247303        MOVE SPAR-IDANSK TO W-IDPERSON                                    
247403        PERFORM IMS-GU-WDP311                                             
247503        IF SEGMENT-FINNS                                                  
247603           MOVE PERS-IDMAIL TO SPAR-IDMAIL1                               
247703        ELSE                                                              
247803           MOVE SPACE       TO SPAR-IDMAIL1                               
247903        END-IF                                                            
248003     END-IF                                                               
248103                                                                          
248203     IF SPAR-IDBERED > +0                                                 
248303        MOVE 'BER     '   TO W-KDARBTYP                                   
248403        MOVE SPAR-IDBERED TO W-IDPERSON                                   
248503        PERFORM IMS-GU-WDP311                                             
248603        IF SEGMENT-FINNS                                                  
248703           MOVE PERS-IDMAIL TO SPAR-IDMAIL2                               
248803        ELSE                                                              
248903           MOVE SPACE       TO SPAR-IDMAIL2                               
249003        END-IF                                                            
249103     END-IF                                                               
249203                                                                          
249303     IF SPAR-IDINK  > +0                                                  
249403        MOVE 'INK     '  TO W-KDARBTYP                                    
249503        MOVE SPAR-IDINK  TO W-IDPERSON                                    
249603        PERFORM IMS-GU-WDP311                                             
249703        IF SEGMENT-FINNS                                                  
249803           MOVE PERS-IDMAIL TO SPAR-IDMAIL3                               
249903        ELSE                                                              
250003           MOVE SPACE       TO SPAR-IDMAIL3                               
250103        END-IF                                                            
250203     END-IF                                                               
250303                                                                          
250403     MOVE 'QUAL' TO W-KDARBTYP                                            
250503                    W-KDARBTYP-B                                          
250603     MOVE NEJ TO SW-TRAEFF                                                
250703     PERFORM IMS-GU-WDP3A                                                 
250803     PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = 'J'                      
250903        IF SEQA-IDARTNR-TOM < W-IDARTNR                                   
251003           PERFORM IMS-GN-WDP3A                                           
251103        ELSE                                                              
251203           IF SEQA-IDARTNR-FOM <= W-IDARTNR                               
251303           AND SEQA-IDARTNR-TOM >= W-IDARTNR                              
251403               MOVE 'J' TO SW-TRAEFF                                      
251503           ELSE                                                           
251603              MOVE 'GE' TO STATUS-WS                                      
251703           END-IF                                                         
251803        END-IF                                                            
251903     END-PERFORM                                                          
252003     IF SW-TRAEFF = 'J'                                                   
252103        MOVE SEQA-IDPERSON TO W-IDPERSON                                  
252203                              SPAR-IDPERSON                               
252303     ELSE                                                                 
252403        MOVE SPAR-IDLEVNR TO W-IDLEVNR-B                                  
252503        PERFORM IMS-GU-WDP3B                                              
252603        IF SEGMENT-FINNS                                                  
252703           MOVE 'J'          TO SW-TRAEFF                                 
252803           MOVE SEQB-IDPERSON TO W-IDPERSON                               
252903                                 SPAR-IDPERSON                            
253003        ELSE                                                              
253103           PERFORM IMS-GU-WDP3C                                           
253203           PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = 'J'                
253303              IF SEQC-IDFKNGRP-TOM < SPAR-IDFKNGRP                        
253403                 PERFORM IMS-GN-WDP3C                                     
253503              ELSE                                                        
253603                 IF SEQC-IDFKNGRP-FOM <= SPAR-IDFKNGRP                    
253703                 AND SEQC-IDFKNGRP-TOM >= SPAR-IDFKNGRP                   
253803                    MOVE 'J' TO SW-TRAEFF                                 
253903                 ELSE                                                     
254003                    MOVE 'GE' TO STATUS-WS                                
254103                 END-IF                                                   
254203              END-IF                                                      
254303           END-PERFORM                                                    
254403           IF SW-TRAEFF = 'J'                                             
254503              MOVE SEQC-IDPERSON TO W-idperson                            
254603                                    spar-idperson                         
254703           END-IF                                                         
254803        END-IF                                                            
254903     END-IF                                                               
255003     PERFORM IMS-GU-WDP311                                                
255103     IF SEGMENT-FINNS                                                     
255203        MOVE PERS-IDMAIL TO SPAR-IDMAIL4                                  
255303     ELSE                                                                 
255403        MOVE SPACE       TO SPAR-IDMAIL4                                  
255503        MOVE ZERO TO SPAR-IDPERSON                                        
255603     END-IF                                                               
255703     .                                                                    
255803     EJECT                                                                
255903 HE-UPD-KOD20-KODDC11 SECTION.                                            
256003                                                                          
256103     PERFORM IMS-GET-UNIK-WLARTC01                                        
256203     PERFORM IMS-GET-HOLD-NEXT-WLARTC11                                   
256303                                                                          
256403     MOVE ARTC-CLAG-WDK611 TO TEST-CLAG-WDK611                            
256503     MOVE TEST-CLAG-KDLEVSP TO SPAR-CLAG-KDLEVSP                          
256603                                                                          
256703*    --- GLOB---KDLEVSP                                                   
256803     IF MID-KDLEVSP-GLOB NOT = ALL '+'                                    
256903       MOVE MID-KDLEVSP-GLOB TO TEST-CLAG-KDLEVSP                         
257003                                MOD-KDLEVSP-GLOB                          
257103       MOVE MSGI-IDUSER        TO ARTC-CLAG-IDUSER-SPKVAL                 
257203       MOVE DAGENS-DATUM       TO ARTC-CLAG-TISPARR-KVAL                  
257303       IF SW-MAIL = JA                                                    
257403          MOVE DAGENS-DATUM    TO ARTC-CLAG-TIMAIL-KVAL                   
257503       END-IF                                                             
257603       IF MID-KDLEVSP-GLOB = ZERO                                         
257703          MOVE ZERO            TO ARTC-CLAG-TIMAIL-KVAL                   
257803       END-IF                                                             
257903       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLEVSP-GLOB-UPD-ATTR            
258003     ELSE                                                                 
258103       MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-GLOB                         
258203     END-IF                                                               
258303                                                                          
258403*    --- DC11 --- KDLEVSP                                                 
258503     IF MID-KDLEVSP-DC11 NOT = ALL '+'                                    
258603       MOVE MID-KDLEVSP-DC11 TO TEST-CLAG-KDLEVSP                         
258703                                MOD-KDLEVSP-DC11                          
258803       MOVE MSGI-IDUSER        TO ARTC-CLAG-IDUSER-SPKVAL                 
258903                                  MOD-IDUSER-SPKVAL-DC11                  
259003       MOVE DAGENS-DATUM       TO ARTC-CLAG-TISPARR-KVAL                  
259103                                  MOD-TISPARR-KVAL-DC11                   
259203       IF SW-MAIL = JA                                                    
259303          MOVE DAGENS-DATUM    TO ARTC-CLAG-TIMAIL-KVAL                   
259403       END-IF                                                             
259503       IF MID-KDLEVSP-DC11 = ZERO                                         
259603          MOVE ZERO            TO ARTC-CLAG-TIMAIL-KVAL                   
259703       END-IF                                                             
259803       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLEVSP-GLOB-UPD-ATTR            
259903     ELSE                                                                 
260003       MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP-DC11                         
260103                                  MOD-TISPARR-KVAL-DC11                   
260203                                  MOD-IDUSER-SPKVAL-DC11                  
260303     END-IF                                                               
260403                                                                          
260503*    --- Kolla om täckning skall utföras på CDC                           
260603     IF TEST-CLAG-KDLEVSP = ZERO                                          
260703       IF ARTC-CLAG-KDLEVSP > ZERO                                        
260803*        --- Leveransspärr släpps i denna uppdatering                     
260903*        --- Skicka av. trans för täckning av lagersaldo                  
261003         PERFORM HA-TAECKNING-FOR-CDC                                     
261103         MOVE ARTC-CLAG-IDDC-REF  TO W-IDDC-REF                           
261203         PERFORM IMS-GHU-WDK629                                           
261303         IF SEGMENT-FINNS                                                 
261403           IF WDK6-CREF-FLREFNYO = JA                                     
261503             MOVE NEJ             TO WDK6-CREF-FLREFNYO                   
261603             PERFORM IMS-REPL-WDK629                                      
261703           END-IF                                                         
261803         END-IF                                                           
261903       END-IF                                                             
262003     END-IF                                                               
262103                                                                          
262203     MOVE TEST-CLAG-KDLEVSP TO ARTC-CLAG-KDLEVSP                          
262303                                                                          
262403*    --- DC11 --- KVSPARR-KVAL                                            
262503     IF MID-KVSPARR-KVAL-DC11 NOT = ALL '+'                               
262603       MOVE MID-KVSPARR-KVAL-DC11 TO ARTC-CLAG-KVSPARR-KVAL               
262703                                     MOD-KVSPARR-KVAL-DC11                
262803       MOVE MSGI-IDUSER           TO ARTC-CLAG-IDUSER-SPKVAL              
262903                                     MOD-IDUSER-SPKVAL-DC11               
263003       MOVE DAGENS-DATUM          TO ARTC-CLAG-TISPARR-KVAL               
263103                                     MOD-TISPARR-KVAL-DC11                
263203       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
263303                                  MOD-KVSPARR-KVAL-DC11-UPD-ATTR          
263403     ELSE                                                                 
263503       MOVE MFS-ROER-EJ-FAELT TO MOD-KVSPARR-KVAL-DC11                    
263603                                  MOD-TISPARR-KVAL-DC11                   
263703                                  MOD-IDUSER-SPKVAL-DC11                  
263803     END-IF                                                               
263903                                                                          
264003                                                                          
264103     PERFORM IMS-REPL-WLARTC11                                            
264203                                                                          
264303     PERFORM IMS-GET-HOLD-NEXT-WLARTC25                                   
264403                                                                          
264503     IF MID-TEARTNOT-GLOB NOT = ALL '+'                                   
264603       IF PERS-CDC                                                        
264703         IF SEGMENT-FINNS                                                 
264803           IF TEST-CLAG-KDLEVSP = ZERO                                    
264903*            --- Spärrkod är nollad, tag bort noten trots input           
265003             MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-GLOB                    
265103             MOVE MFS-FORMATETS-ATTR TO MOD-TEARTNOT-GLOB-ATTR            
265203             PERFORM IMS-DLET-WLARTC25                                    
265303           ELSE                                                           
265403             MOVE SPACE TO WS-TEARTNOT-GLOB                               
265503             MOVE MID-TEARTNOT-GLOB TO WS-TEARTNOT-GLOB                   
265603             MOVE WS-TEARTNOT-GLOB TO ARTC-NOT-TEARTNOT                   
265703                                      MOD-TEARTNOT-GLOB                   
265803             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
265903                                       MOD-TEARTNOT-GLOB-ATTR             
266003             PERFORM IMS-REPL-WLARTC25                                    
266103           END-IF                                                         
266203         ELSE                                                             
266303           IF TEST-CLAG-KDLEVSP > ZERO                                    
266403           AND MID-TEARTNOT-GLOB NOT = SPACE                              
266503             MOVE SPACE TO WS-TEARTNOT-GLOB                               
266603             MOVE MID-TEARTNOT-GLOB TO WS-TEARTNOT-GLOB                   
266703             MOVE +4                TO ARTC-NOT-KDNOTTYP                  
266803             MOVE WS-TEARTNOT-GLOB  TO ARTC-NOT-TEARTNOT                  
266903                                       MOD-TEARTNOT-GLOB                  
267003             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
267103                                       MOD-TEARTNOT-GLOB-ATTR             
267203             PERFORM IMS-ISRT-WLARTC25                                    
267303           ELSE                                                           
267403             MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-GLOB                    
267503           END-IF                                                         
267603         END-IF                                                           
267703       ELSE                                                               
267803         MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-GLOB                        
267903       END-IF                                                             
268003     ELSE                                                                 
268103       IF SEGMENT-FINNS                                                   
268203         IF TEST-CLAG-KDLEVSP = ZERO                                      
268303*          --- Noten FÅR EJ FINNAS i detta läge                           
268403           MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-GLOB                      
268503           MOVE MFS-FORMATETS-ATTR TO MOD-TEARTNOT-GLOB-ATTR              
268603           PERFORM IMS-DLET-WLARTC25                                      
268703         ELSE                                                             
268803           MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-GLOB                    
268903           PERFORM IMS-REPL-WLARTC25                                      
269003         END-IF                                                           
269103       END-IF                                                             
269203     END-IF                                                               
269303** REPLACE WDGX2404 OR DELETE!!                                           
269403                                                                          
269503     PERFORM IMS-GU-WDK611                                                
269603                                                                          
269703       IF SEGMENT-FINNS                                                   
269902         MOVE ARTC-CLAG-KDLEVSP TO WS-NEW-KDLEVSP                         
270002         MOVE ARTC-CLAG-KVSPARR-KVAL TO                                   
270102                                   WS-NEW-KVSPARR-KVAL                    
270202                                                                          
270502         IF WS-NEW-KDLEVSP = 0 AND WS-NEW-KVSPARR-KVAL = 0                
270503            MOVE '11'        TO W-IDDC-KY-MIN                             
270504                                W-IDDC-KY-MAX                             
270603            MOVE W-IDARTNR-X TO W-IDARTNR-MIN                             
270604                                W-IDARTNR-MAX                             
270605                                                                          
270606            PERFORM IMS-GHU-WDGX2404                                      
270607            IF SEGMENT-FINNS                                              
270608               PERFORM IMS-DLET-2404                                      
270612            END-IF                                                        
270613                                                                          
270614         ELSE                                                             
270615                                                                          
270616            IF WS-NEW-KDLEVSP = 0 OR WS-NEW-KVSPARR-KVAL = 0              
270626                                                                          
270627               MOVE '11'        TO W-IDDC-KY-MIN                          
270628                                   W-IDDC-KY-MAX                          
270629               MOVE W-IDARTNR-X TO W-IDARTNR-MIN                          
270630                                   W-IDARTNR-MAX                          
270631                                                                          
270632               PERFORM IMS-GHU-WDGX2404                                   
270633               IF SEGMENT-FINNS                                           
270634                  IF WS-NEW-KDLEVSP = 0                                   
270635                     MOVE WS-NEW-KDLEVSP TO 2404-KDLEVSP                  
270636                  END-IF                                                  
270637                  IF WS-NEW-KVSPARR-KVAL = 0                              
270638                     MOVE WS-NEW-KVSPARR-KVAL TO 2404-KVSPARR-KVAL        
270639                  END-IF                                                  
270640                                                                          
270641                  PERFORM IMS-REPL-2404                                   
270642               END-IF                                                     
270643                                                                          
270644            END-IF                                                        
270645         END-IF                                                           
270646       END-IF                                                             
270647                                                                          
270650* ---------------------------------- ARTC KLART                           
271300* Nu innehåller ARTC11-arean gällande basvärden                           
271310                                                                          
271320     PERFORM IMS-GET-UNIK-WLARTS01                                        
271330                                                                          
271400     IF SEGMENT-FINNS                                                     
271500       PERFORM IMS-GET-HOLD-NEXT-WLARTS11                                 
271600       IF SEGMENT-FINNS                                                   
271700         MOVE ARTS-SLAG-IDDC TO SPAR-IDDC2                                
271800         MOVE SPAR-IDDC-TAB(2) TO SPAR-IDDC-LEVEL3                        
271900       END-IF                                                             
272000                                                                          
272100       PERFORM UNTIL SEGMENT-SAKNAS                                       
272200         MOVE ARTS-SLAG-WDK711 TO TEST-SLAG-WDK711                        
272300* -------------------------------------------- GLOBALT                    
272400*        --- Flytta först GLOBAL-värden                                   
272500         IF MID-KDLEVSP-GLOB = ALL '+'                                    
272600           CONTINUE                                                       
272700         ELSE                                                             
272800           MOVE MID-KDLEVSP-GLOB TO TEST-SLAG-KDLEVSP                     
272900           MOVE MSGI-IDUSER      TO ARTS-SLAG-IDUSER-SPKVAL               
273000           MOVE DAGENS-DATUM     TO ARTS-SLAG-TISPARR-KVAL                
273100         END-IF                                                           
273200*        --- Kolla sen CDC-värden                                         
273300         IF MID-KDLEVSP-DC11 = ALL '+'                                    
273400           CONTINUE                                                       
273500         ELSE                                                             
273600*          --- Kod på CDC som ev. skall slå över övriga DC                
273700*          --- ifall de har kod 20 satt                                   
273800           IF MID-KDLEVSP-DC11 = 21                                       
273900             IF ARTS-SLAG-KDLEVSP = 20                                    
274000             AND MID-KDLEVSP-GLOB = ALL '+'                               
274100               MOVE MID-KDLEVSP-DC11 TO ARTS-SLAG-KDLEVSP                 
274200                                        TEST-SLAG-KDLEVSP                 
274300               MOVE MSGI-IDUSER      TO ARTS-SLAG-IDUSER-SPKVAL           
274400               MOVE DAGENS-DATUM     TO ARTS-SLAG-TISPARR-KVAL            
274500             END-IF                                                       
274600           ELSE                                                           
274700*            --- KDLEVSP=0 på CDC skall inte påverka SDC och NDC          
274800             CONTINUE                                                     
274900           END-IF                                                         
275000         END-IF                                                           
275100* -------------------------------------------- LOKALT                     
275200         MOVE ARTS-SLAG-IDDC TO WS-IDDC                                   
275300                                 W-IDDC                                   
275400         IF TEST-CLAG-KDLEVSP = 20 OR ZERO                                
275500           IF TEST-SLAG-KDLEVSP NOT = ARTS-SLAG-KDLEVSP                   
275600              MOVE TEST-SLAG-KDLEVSP TO ARTS-SLAG-KDLEVSP                 
275700              MOVE MSGI-IDUSER       TO ARTS-SLAG-IDUSER-SPKVAL           
275800              MOVE DAGENS-DATUM      TO ARTS-SLAG-TISPARR-KVAL            
275900              IF ARTS-SLAG-FLREFNYO = JA                                  
276000                 MOVE NEJ            TO ARTS-SLAG-FLREFNYO                
276100              END-IF                                                      
276200           END-IF                                                         
276300           IF ARTS-SLAG-KDLEVSP       = ZERO                              
276400           AND ARTS-SLAG-KVSPARR-KVAL = ZERO                              
276500              MOVE SPACE TO TEST-SLAG-TEKVAL                              
276600           END-IF                                                         
276700           IF TEST-SLAG-TEKVAL NOT = ARTS-SLAG-TEKVAL                     
276800              MOVE TEST-SLAG-TEKVAL TO ARTS-SLAG-TEKVAL                   
276900              MOVE MSGI-IDUSER      TO ARTS-SLAG-IDUSER-SPKVAL            
277000              MOVE DAGENS-DATUM     TO ARTS-SLAG-TISPARR-KVAL             
277100           END-IF                                                         
277200         END-IF                                                           
277300                                                                          
277310** CHECK HERE!!                                                           
277400         PERFORM IMS-REPL-WLARTS11                                        
277410         PERFORM HFB-CHECK-REPL-WGX2404                                   
277500         PERFORM IMS-GET-HOLD-NEXT-WLARTS11                               
277600       END-PERFORM                                                        
277700     END-IF                                                               
277800     .                                                                    
277900     EJECT                                                                
278000 HF-UPD-SDC SECTION.                                                      
278100                                                                          
278200     PERFORM IMS-GET-UNIK-WLARTC01                                        
278300     PERFORM IMS-GET-HOLD-NEXT-WLARTC11                                   
278400                                                                          
278500     MOVE ARTC-CLAG-WDK611 TO TEST-CLAG-WDK611                            
278600     MOVE TEST-CLAG-KDLEVSP TO SPAR-CLAG-KDLEVSP                          
278700                                                                          
278800     PERFORM IMS-GET-UNIK-WLARTS01                                        
278900                                                                          
279000     IF SEGMENT-FINNS                                                     
279100       MOVE +1 TO RAD-IX                                                  
279200       MOVE SPAR-IDDC-TAB(RAD-IX) TO W-IDDC                               
279300       MOVE SPAR-IDDC-TAB(2) TO SPAR-IDDC-LEVEL3                          
279400       PERFORM IMS-GET-HOLD-UNIK-WLARTS11                                 
279500                                                                          
279600       PERFORM UNTIL RAD-IX > RAD-IX-MAX or segment-saknas                
279700         MOVE ARTS-SLAG-WDK711 TO TEST-SLAG-WDK711                        
279800                                                                          
279900         MOVE NEJ               TO STOCK-BALANCE-SW                       
280000         IF ARTS-SLAG-KVLS      >  ZERO                                   
280100         OR ARTS-SLAG-KVEFRS    >  ZERO                                   
280200         OR ARTS-SLAG-KVAKS-SDC >  ZERO                                   
280300         OR ARTS-SLAG-KVAKS-PAV >  ZERO                                   
280400            MOVE JA             TO STOCK-BALANCE-SW                       
280500         END-IF                                                           
280600* -------------------------------------------- GLOBALT                    
280700*        --- Flytta först GLOBAL-värden                                   
280800         IF MID-KDLEVSP-GLOB = ALL '+'                                    
280900           CONTINUE                                                       
281000         ELSE                                                             
281100           MOVE MID-KDLEVSP-GLOB TO TEST-SLAG-KDLEVSP                     
281200           MOVE MSGI-IDUSER      TO ARTS-SLAG-IDUSER-SPKVAL               
281300           MOVE DAGENS-DATUM     TO ARTS-SLAG-TISPARR-KVAL                
281400         END-IF                                                           
281500*        --- Kolla sen CDC-värden                                         
281600         IF MID-KDLEVSP-DC11 = ALL '+'                                    
281700           CONTINUE                                                       
281800         ELSE                                                             
281900*          --- Kod på CDC som ev. skall slå över övriga DC                
282000*          --- ifall de har kod 20 satt                                   
282100           IF MID-KDLEVSP-DC11 = 21                                       
282200             IF ARTS-SLAG-KDLEVSP = 20                                    
282300             AND MID-KDLEVSP-GLOB = ALL '+'                               
282400               MOVE MID-KDLEVSP-DC11 TO TEST-SLAG-KDLEVSP                 
282500               MOVE MSGI-IDUSER      TO ARTS-SLAG-IDUSER-SPKVAL           
282600               MOVE DAGENS-DATUM     TO ARTS-SLAG-TISPARR-KVAL            
282700             END-IF                                                       
282800           ELSE                                                           
282900*            --- KDLEVSP=0 på CDC skall inte påverka SDC och NDC          
283000             CONTINUE                                                     
283100           END-IF                                                         
283200         END-IF                                                           
283300* -------------------------------------------- LOKALT                     
283400         MOVE ARTS-SLAG-IDDC TO WS-IDDC                                   
283500                                 W-IDDC                                   
283600         PERFORM HG-KOLLA-LEVEL                                           
283700                                                                          
283800         IF PERS-CDC                                                      
283900         OR ((WS-LEVEL = 2) AND                                           
284000             ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR                  
284100             (MSGI-IDUSER = W-IDUSER1) OR                                 
284200             (MSGI-IDUSER = W-IDUSER2) OR                                 
284300             (MSGI-IDUSER = W-IDUSER3) OR                                 
284400             (MSGI-IDUSER = W-IDUSER4)))                                  
284500         OR ((WS-LEVEL = 3) AND                                           
284600             ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR                  
284700             (PERS-WS-IDDC = 6332-KEY-FB-AREA-IDDC)   OR                  
284800             (MSGI-IDUSER = W-IDUSER1) OR                                 
284900             (MSGI-IDUSER = W-IDUSER2) OR                                 
285000             (MSGI-IDUSER = W-IDUSER3) OR                                 
285100             (MSGI-IDUSER = W-IDUSER4)))                                  
285200*          --- Endast CDC-folk + eget DC-folk får uppdatera               
285300*          --- flytta ev. input till test                                 
285400           IF MID-KDLEVSP(RAD-IX) NOT = ALL '+'                           
285500*            --- Uppdat av SDC endast godk med 0 i global                 
285600             MOVE MID-KDLEVSP(RAD-IX) TO TEST-SLAG-KDLEVSP                
285700           END-IF                                                         
285800*          --- flytta data till utfält och bas                            
285900           IF TEST-SLAG-KDLEVSP NOT = ARTS-SLAG-KDLEVSP                   
286000             IF  TEST-SLAG-KDLEVSP  = 22                                  
286100             AND STOCK-BAL-NO                                             
286200                 CONTINUE                                                 
286300             ELSE                                                         
286400              MOVE TEST-SLAG-KDLEVSP    TO MOD-KDLEVSP(RAD-IX)            
286500                                           ARTS-SLAG-KDLEVSP              
286600              MOVE MSGI-IDUSER          TO ARTS-SLAG-IDUSER-SPKVAL        
286700              MOVE DAGENS-DATUM         TO ARTS-SLAG-TISPARR-KVAL         
286800              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
286900                                   MOD-KDLEVSP-UPD-ATTR(RAD-IX)           
287000             END-IF                                                       
287100                                                                          
287200             IF WS-LEVEL = 2                                              
287300              IF WS-SHOW  = 'N'                                           
287400                PERFORM HFA-UPD-LEVEL3-DC                                 
287500              END-IF                                                      
287600             END-IF                                                       
287700             IF ARTS-SLAG-FLREFNYO = JA                                   
287800                 MOVE NEJ           TO ARTS-SLAG-FLREFNYO                 
287900             END-IF                                                       
288000           ELSE                                                           
288100             MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVSP(RAD-IX)                
288200           END-IF                                                         
288300           IF NDC                                                         
288400*            --- Kolla om täckning skall utföras på NDC                   
288500             IF TEST-SLAG-KDLEVSP = ZERO                                  
288600               IF ARTS-SLAG-KDLEVSP > ZERO                                
288700*              --- Leveransspärr släpps i denna uppdatering               
288800*              --- Skicka av. trans för täckning av lagersaldo            
288900                 PERFORM HB-TAECKNING-FOR-NDC                             
289000               END-IF                                                     
289100             END-IF                                                       
289200           END-IF                                                         
289300* ---------------- flytta ev. input till test                             
289400           IF MID-KVSPARR-KVAL(RAD-IX) NOT = ALL '+'                      
289500             IF TEST-SLAG-KDLEVSP = ZERO                                  
289600               MOVE MID-KVSPARR-KVAL(RAD-IX) TO                           
289700                                    TEST-SLAG-KVSPARR-KVAL                
289800             END-IF                                                       
289900           END-IF                                                         
290000*          --- flytta data till utfält och bas                            
290100           IF TEST-SLAG-KVSPARR-KVAL NOT =                                
290200                                     ARTS-SLAG-KVSPARR-KVAL               
290300             MOVE TEST-SLAG-KVSPARR-KVAL TO                               
290400                                    ARTS-SLAG-KVSPARR-KVAL                
290500                                    MOD-KVSPARR-KVAL(RAD-IX)              
290600             MOVE MSGI-IDUSER        TO ARTS-SLAG-IDUSER-SPKVAL           
290700             MOVE DAGENS-DATUM       TO ARTS-SLAG-TISPARR-KVAL            
290800             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
290900                           MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)              
291000             IF ARTS-SLAG-FLREFNYO    = JA                                
291100                MOVE NEJ             TO ARTS-SLAG-FLREFNYO                
291200             END-IF                                                       
291300           ELSE                                                           
291400             MOVE MFS-ROER-EJ-FAELT TO                                    
291500                                    MOD-KVSPARR-KVAL(RAD-IX)              
291600           END-IF                                                         
291700* ---------------- uppdatera TEKVAL                                       
291800           IF ARTS-SLAG-KDLEVSP           = ZERO                          
291900           AND ARTS-SLAG-KVSPARR-KVAL = ZERO                              
292000*            --- text skall bort                                          
292100             MOVE SPACE TO TEST-SLAG-TEKVAL                               
292200           ELSE                                                           
292300*            --- förbered uppdat av ny text                               
292400             IF MID-TEKVAL(RAD-IX) NOT = ALL '+'                          
292500               MOVE MID-TEKVAL(RAD-IX) TO TEST-SLAG-TEKVAL                
292600             END-IF                                                       
292700           END-IF                                                         
292800         END-IF                                                           
292900*                                                                         
293000         IF PERS-CDC                                                      
293100         OR ((WS-LEVEL = 2) AND                                           
293200             ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR                  
293300             (MSGI-IDUSER = W-IDUSER1) OR                                 
293400             (MSGI-IDUSER = W-IDUSER2) OR                                 
293500             (MSGI-IDUSER = W-IDUSER3) OR                                 
293600             (MSGI-IDUSER = W-IDUSER4)))                                  
293700         OR ((WS-LEVEL = 3) AND                                           
293800             ((PERS-WS-IDDC = SPAR-IDDC-TAB(RAD-IX))  OR                  
293900             (PERS-WS-IDDC = 6332-KEY-FB-AREA-IDDC) OR                    
294000             (MSGI-IDUSER = W-IDUSER1) OR                                 
294100             (MSGI-IDUSER = W-IDUSER2) OR                                 
294200             (MSGI-IDUSER = W-IDUSER3) OR                                 
294300             (MSGI-IDUSER = W-IDUSER4)))                                  
294400           IF TEST-SLAG-TEKVAL NOT = ARTS-SLAG-TEKVAL                     
294500             IF TEST-SLAG-KDLEVSP = 22 AND PERS-CDC-SE                    
294600               MOVE MFS-ROER-EJ-FAELT TO MOD-TEKVAL(RAD-IX)               
294700*           --- Endast folk från det egna lagret får uppdatera            
294800*            --- ny TEKVAL                                                
294900             ELSE                                                         
295000               MOVE TEST-SLAG-TEKVAL TO ARTS-SLAG-TEKVAL                  
295100                                        MOD-TEKVAL(RAD-IX)                
295200               MOVE MSGI-IDUSER      TO ARTS-SLAG-IDUSER-SPKVAL           
295300               MOVE DAGENS-DATUM TO ARTS-SLAG-TISPARR-KVAL                
295400               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
295500                                      MOD-TEKVAL-ATTR(RAD-IX)             
295600             END-IF                                                       
295700           ELSE                                                           
295800             MOVE MFS-ROER-EJ-FAELT TO MOD-TEKVAL(RAD-IX)                 
295900           END-IF                                                         
296000         END-IF                                                           
296100         IF NOT NDC                                                       
296200           MOVE ARTS-SLAG-IDUSER-SPKVAL TO                                
296300                                    MOD-IDUSER-SPKVAL(RAD-IX)             
296400          MOVE ARTS-SLAG-TISPARR-KVAL TO                                  
296500                                    MOD-TISPARR-KVAL(RAD-IX)              
296600         END-IF                                                           
296700         PERFORM IMS-REPL-WLARTS11                                        
296710         PERFORM HFB-CHECK-REPL-WGX2404                                   
296800         ADD +1 TO RAD-IX                                                 
296900         IF RAD-IX <= RAD-IX-MAX                                          
297000           MOVE SPAR-IDDC-TAB(RAD-IX) TO W-IDDC                           
297100           PERFORM IMS-GET-HOLD-UNIK-WLARTS11                             
297200         END-IF                                                           
297300       END-PERFORM                                                        
297400     END-IF                                                               
297500     .                                                                    
297600     EJECT                                                                
297700                                                                          
297800 HFA-UPD-LEVEL3-DC SECTION.                                               
297900                                                                          
298000     MOVE ARTS-SLAG-IDDC    TO W-6332-IDDC                                
298100     PERFORM IMS-GU-WDGX6332                                              
298200     IF SEGMENT-FINNS                                                     
298300       PERFORM IMS-GNP-WDGX6334                                           
298400       PERFORM UNTIL SEGMENT-SAKNAS                                       
298500         MOVE 6334-IDDC   TO W-IDDC-K7                                    
298600                                                                          
298700         MOVE NEJ              TO STOCK-BALANCE-SW                        
298800                                                                          
298900         PERFORM IMS-GHU-WDK711                                           
299000         IF SEGMENT-FINNS                                                 
299100           IF WDK7-SLAG-KVLS       >  ZERO                                
299200           OR WDK7-SLAG-KVEFRS     >  ZERO                                
299300           OR WDK7-SLAG-KVAKS-SDC  >  ZERO                                
299400           OR WDK7-SLAG-KVAKS-PAV  >  ZERO                                
299500              MOVE JA              TO STOCK-BALANCE-SW                    
299600           END-IF                                                         
299700           IF  TEST-SLAG-KDLEVSP   =  22                                  
299800           AND STOCK-BAL-NO                                               
299900               CONTINUE                                                   
300000           ELSE                                                           
300100             MOVE TEST-SLAG-KDLEVSP    TO WDK7-SLAG-KDLEVSP               
300200             MOVE MSGI-IDUSER          TO WDK7-SLAG-IDUSER-SPKVAL         
300300             MOVE DAGENS-DATUM         TO WDK7-SLAG-TISPARR-KVAL          
300400             PERFORM IMS-REPL-WDK711                                      
300500           END-IF                                                         
300600         END-IF                                                           
300700         PERFORM IMS-GNP-WDGX6334                                         
300800       END-PERFORM                                                        
300900     END-IF                                                               
301000     .                                                                    
301100     EJECT                                                                
301200                                                                          
301210 HFB-CHECK-REPL-WGX2404 SECTION.                                          
301220                                                                          
301240         MOVE ARTS-SLAG-KDLEVSP TO WS-NEW-KDLEVSP                         
301250         MOVE ARTS-SLAG-KVSPARR-KVAL TO                                   
301260                                   WS-NEW-KVSPARR-KVAL                    
301270                                                                          
301280         IF WS-NEW-KDLEVSP = 0 AND WS-NEW-KVSPARR-KVAL = 0                
301290            MOVE W-IDDC      TO W-IDDC-KY-MIN                             
301300                                W-IDDC-KY-MAX                             
301310            MOVE W-IDARTNR-X TO W-IDARTNR-MIN                             
301311                                W-IDARTNR-MAX                             
301312                                                                          
301313            PERFORM IMS-GHU-WDGX2404                                      
301314            IF SEGMENT-FINNS                                              
301315               PERFORM IMS-DLET-2404                                      
301316            END-IF                                                        
301317                                                                          
301318         ELSE                                                             
301319                                                                          
301320            IF WS-NEW-KDLEVSP = 0 OR WS-NEW-KVSPARR-KVAL = 0              
301331                                                                          
301332                                                                          
301333               MOVE W-IDDC      TO W-IDDC-KY-MIN                          
301334                                   W-IDDC-KY-MAX                          
301335               MOVE W-IDARTNR-X TO W-IDARTNR-MIN                          
301336                                   W-IDARTNR-MAX                          
301337                                                                          
301338               PERFORM IMS-GHU-WDGX2404                                   
301339               IF SEGMENT-FINNS                                           
301340                  IF WS-NEW-KDLEVSP = 0                                   
301341                    MOVE WS-NEW-KDLEVSP TO 2404-KDLEVSP                   
301342                  END-IF                                                  
301343                  IF WS-NEW-KVSPARR-KVAL = 0                              
301344                    MOVE WS-NEW-KVSPARR-KVAL TO 2404-KVSPARR-KVAL         
301345                  END-IF                                                  
301346                  PERFORM IMS-REPL-2404                                   
301347               END-IF                                                     
301348            END-IF                                                        
301349         END-IF                                                           
301350                                                                          
301351     .                                                                    
301352     EJECT                                                                
301353                                                                          
301360 HG-KOLLA-LEVEL SECTION.                                                  
301400                                                                          
301500     PERFORM IMS-GU-WDGX6331                                              
301600     IF SEGMENT-FINNS                                                     
301700       IF W-IDDC = '11'                                                   
301800         MOVE +1 TO WS-LEVEL                                              
301900       ELSE                                                               
302000         MOVE W-IDDC  TO W-6332-IDDC                                      
302100         PERFORM IMS-GNP-WDGX6332                                         
302200         IF SEGMENT-FINNS                                                 
302300           MOVE +2 TO WS-LEVEL                                            
302400         ELSE                                                             
302500           MOVE W-IDDC TO W-6334-IDDC                                     
302600           PERFORM IMS-GU-WDGX6331                                        
302700           PERFORM IMS-GNP-WDGX6334-2                                     
302800           IF SEGMENT-FINNS                                               
302900             MOVE +3 TO WS-LEVEL                                          
303000           END-IF                                                         
303100         END-IF                                                           
303200       END-IF                                                             
303300     ELSE                                                                 
303400       MOVE ZERO TO WS-LEVEL                                              
303500     END-IF                                                               
303600     .                                                                    
303700     EJECT                                                                
303800 MFS-RENSA-FAELT-UT SECTION.                                              
303900     SKIP2                                                                
304000*    --- ALLA UTDATA-FÄLT                                                 
304100                                                                          
304200     MOVE MFS-RENSA-FAELT         TO MOD-BEART                            
304300                                     MOD-TEARTNOT-GLOB                    
304400                                     MOD-KDLEVSP-GLOB                     
304500                                     MOD-IDDC-DC11                        
304600                                     MOD-KDLEVSP-DC11                     
304700                                     MOD-KVSPARR-KVAL-DC11                
304800                                     MOD-TISPARR-KVAL-DC11                
304900                                     MOD-IDUSER-SPKVAL-DC11               
305000                                     MOD-TIMAIL-KVAL                      
305100                                     MOD-FLTEXT                           
305200     MOVE +1 TO RAD-IX                                                    
305300     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
305400       MOVE MFS-RENSA-FAELT       TO MOD-IDUSER-SPKVAL(RAD-IX)            
305500                                     MOD-TISPARR-KVAL(RAD-IX)             
305600                                     MOD-KDLEVSP(RAD-IX)                  
305700                                     MOD-KVSPARR-KVAL(RAD-IX)             
305800                                     MOD-TEKVAL(RAD-IX)                   
305900                                     MOD-KVLS(RAD-IX)                     
306000                                     MOD-IDDC(RAD-IX)                     
306100                                     MOD-FLDCLVL3(RAD-IX)                 
306200                                     MOD-FLSPARR(RAD-IX)                  
306300       ADD +1 TO RAD-IX                                                   
306400     END-PERFORM                                                          
306500                                                                          
306600                                                                          
306700     .                                                                    
306800     EJECT                                                                
306900                                                                          
307000 MFS-RENSA-FAELT-UT-CDC SECTION.                                          
307100     SKIP2                                                                
307200     MOVE MFS-RENSA-FAELT  TO  MOD-KDLEVSP-GLOB                           
307300                               MOD-IDDC-DC11                              
307400                               MOD-KDLEVSP-DC11                           
307500                               MOD-KVSPARR-KVAL-DC11                      
307600                               MOD-TISPARR-KVAL-DC11                      
307700                               MOD-IDUSER-SPKVAL-DC11                     
307800                               MOD-TIMAIL-KVAL                            
307900                               MOD-FLMAIL                                 
308000     .                                                                    
308100     EJECT                                                                
308200                                                                          
308300 MFS-RENSA-FAELT-UT-SDC-NDC SECTION.                                      
308400     SKIP2                                                                
308500*    --- ALLA SPECIFIKA UTDATA-FÄLT FÖR SDC OCH NDC                       
308600     MOVE +1 TO RAD-IX                                                    
308700     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
308800       MOVE MFS-RENSA-FAELT       TO MOD-IDUSER-SPKVAL(RAD-IX)            
308900                                     MOD-TISPARR-KVAL(RAD-IX)             
309000                                     MOD-KDLEVSP(RAD-IX)                  
309100                                     MOD-KVSPARR-KVAL(RAD-IX)             
309200                                     MOD-KVLS(RAD-IX)                     
309300                                     MOD-IDDC(RAD-IX)                     
309400                                     MOD-FLDCLVL3(RAD-IX)                 
309500                                     MOD-FLSPARR(RAD-IX)                  
309600       ADD +1 TO RAD-IX                                                   
309700     END-PERFORM                                                          
309800     .                                                                    
309900     EJECT                                                                
310000 MFS-RENSA-FAELT-IN-UT-SDC-NDC SECTION.                                   
310100     SKIP2                                                                
310200*    --- ALLA KOMBINERDE IN-/UT-DATA-FÄLT FÖR SDC OCH NDC                 
310300     MOVE +1 TO RAD-IX                                                    
310400     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
310500       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAL(RAD-IX)                   
310600       ADD +1 TO RAD-IX                                                   
310700     END-PERFORM                                                          
310800     .                                                                    
310900     EJECT                                                                
311000                                                                          
311100 MFS-RENSA-FAELT-IN-CDC SECTION.                                          
311200     SKIP2                                                                
311300*    --- ALLA INDATA-FÄLT                                                 
311400     MOVE MFS-RENSA-FAELT   TO   MOD-KDLEVSP-GLOB-UPD                     
311500                                 MOD-KDLEVSP-DC11-UPD                     
311600                                 MOD-KVSPARR-KVAL-DC11-UPD                
311700                                 MOD-FLMAIL                               
311800     .                                                                    
311900     EJECT                                                                
312000                                                                          
312100 MFS-RENSA-FAELT-IN-SDC-NDC SECTION.                                      
312200     SKIP2                                                                
312300*    --- ALLA INDATA-FÄLT                                                 
312400     MOVE +1 TO RAD-IX                                                    
312500     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
312600       MOVE MFS-RENSA-FAELT       TO MOD-KDLEVSP-UPD(RAD-IX)              
312700                                     MOD-KVSPARR-KVAL-UPD(RAD-IX)         
312800       ADD +1 TO RAD-IX                                                   
312900     END-PERFORM                                                          
313000     .                                                                    
313100     EJECT                                                                
313200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
313300                                                                          
313400*    --- ALLA UTDATA-FÄLT                                                 
313500     MOVE MFS-ROER-EJ-FAELT       TO MOD-BEART                            
313600                                     MOD-TEARTNOT-GLOB                    
313700                                     MOD-KDLEVSP-GLOB                     
313800                                     MOD-IDDC-DC11                        
313900                                     MOD-KDLEVSP-DC11                     
314000                                     MOD-KVSPARR-KVAL-DC11                
314100                                     MOD-TISPARR-KVAL-DC11                
314200                                     MOD-IDUSER-SPKVAL-DC11               
314300                                     MOD-TIMAIL-KVAL                      
314400                                     MOD-FLTEXT                           
314500     MOVE +1 TO RAD-IX                                                    
314600     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
314700       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDUSER-SPKVAL(RAD-IX)            
314800                                     MOD-TISPARR-KVAL(RAD-IX)             
314900                                     MOD-KDLEVSP(RAD-IX)                  
315000                                     MOD-KVSPARR-KVAL(RAD-IX)             
315100                                     MOD-TEKVAL(RAD-IX)                   
315200                                     MOD-KVLS(RAD-IX)                     
315300                                     MOD-IDDC(RAD-IX)                     
315400                                     MOD-FLDCLVL3(RAD-IX)                 
315500                                     MOD-FLSPARR(RAD-IX)                  
315600       ADD +1 TO RAD-IX                                                   
315700     END-PERFORM                                                          
315800     .                                                                    
315900     EJECT                                                                
316000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
316100                                                                          
316200*    --- ALLA INDATA-FÄLT                                                 
316300     MOVE MFS-ROER-EJ-FAELT       TO MOD-TEARTNOT-GLOB                    
316400                                     MOD-KDLEVSP-GLOB-UPD                 
316500                                     MOD-KDLEVSP-DC11-UPD                 
316600                                     MOD-KVSPARR-KVAL-DC11-UPD            
316700                                     MOD-FLMAIL                           
316800     MOVE +1 TO RAD-IX                                                    
316900     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
317000       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDLEVSP-UPD(RAD-IX)              
317100                                     MOD-KVSPARR-KVAL-UPD(RAD-IX)         
317200                                     MOD-TEKVAL(RAD-IX)                   
317300       IF SPAR-CLOSE-TAB(RAD-IX) = 'Y'                                    
317400         MOVE MFS-CLOSE-FIELD TO MOD-KVSPARR-KVAL-UPD-ATTR(RAD-IX)        
317500       END-IF                                                             
317600       ADD +1 TO RAD-IX                                                   
317700     END-PERFORM                                                          
317800     .                                                                    
317900     EJECT                                                                
318000                                                                          
318100                                                                          
318200* --- IMS SEKTIONER ---                                                   
318300     SKIP3                                                                
318400 IMS-GET-MSG SECTION.                                                     
318500                                                                          
318600     MOVE '  QC' TO GODK-STATUSKODER                                      
318700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
318800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
318900     PERFORM IMS-STATUSKONTROLL                                           
319000     .                                                                    
319100     SKIP3                                                                
319200                                                                          
319300 IMS-INSERT-MSG SECTION.                                                  
319400                                                                          
319500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
319600     MOVE SPACE TO GODK-STATUSKODER                                       
319700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
319800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
319900     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
320100     SKIP2                                                                
320200 IMS-INSERT-ALTMSG SECTION.                                               
320300     MOVE SPACE TO GODK-STATUSKODER                                       
320400     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
320500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
320600     PERFORM IMS-STATUSKONTROLL                                           
320700     .                                                                    
320800     EJECT                                                                
320902 IMS-GU-WDK611 SECTION.                                                   
321002                                                                          
321102     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
321202          DELIMITED BY SIZE INTO SSA1                                     
321302     MOVE 'WDK611   ' TO SSA2                                             
321402     MOVE '  GE' TO GODK-STATUSKODER                                      
321502     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
321602     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
321702     PERFORM IMS-STATUSKONTROLL                                           
321802     .                                                                    
321902     EJECT                                                                
322000 IMS-GET-WLARTC01-TEST SECTION.                                           
322100                                                                          
322200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
322300          DELIMITED BY SIZE INTO SSA1                                     
322400     MOVE '  GE' TO GODK-STATUSKODER                                      
322500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
322600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
322700     PERFORM IMS-STATUSKONTROLL                                           
322800     .                                                                    
322900     EJECT                                                                
323000                                                                          
323100 IMS-GET-UNIK-WLARTC01 SECTION.                                           
323200                                                                          
323300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
323400          DELIMITED BY SIZE INTO SSA1                                     
323500     MOVE '  ' TO GODK-STATUSKODER                                        
323600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
323700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
323800     PERFORM IMS-STATUSKONTROLL                                           
323900     .                                                                    
324000     EJECT                                                                
324100                                                                          
324200 IMS-GET-NEXT-WLARTC11 SECTION.                                           
324300                                                                          
324400     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
324500          DELIMITED BY SIZE INTO SSA1                                     
324600     MOVE '  ' TO GODK-STATUSKODER                                        
324700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
324800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
324900     PERFORM IMS-STATUSKONTROLL                                           
325000     .                                                                    
325100     EJECT                                                                
325200                                                                          
325300 IMS-GET-HOLD-NEXT-WLARTC11 SECTION.                                      
325400                                                                          
325500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
325600          DELIMITED BY SIZE INTO SSA1                                     
325700     MOVE '  GE' TO GODK-STATUSKODER                                      
325800     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC11 SSA1                
325900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
326000     PERFORM IMS-STATUSKONTROLL                                           
326100     .                                                                    
326200     EJECT                                                                
326300                                                                          
326400 IMS-REPL-WLARTC11 SECTION.                                               
326500                                                                          
326600     MOVE '  ' TO GODK-STATUSKODER                                        
326700     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
326800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
326900     PERFORM IMS-STATUSKONTROLL                                           
327000     .                                                                    
327100     EJECT                                                                
327200                                                                          
327300 IMS-GET-WLARTC25 SECTION.                                                
327400                                                                          
327500     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
327600          DELIMITED BY SIZE INTO SSA1                                     
327700     MOVE '  GE' TO GODK-STATUSKODER                                      
327800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC25 SSA1                 
327900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
328000     PERFORM IMS-STATUSKONTROLL                                           
328100     .                                                                    
328200     EJECT                                                                
328300                                                                          
328400 IMS-GET-HOLD-NEXT-WLARTC25 SECTION.                                      
328500                                                                          
328600     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
328700          DELIMITED BY SIZE INTO SSA1                                     
328800     MOVE '  GE' TO GODK-STATUSKODER                                      
328900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC25 SSA1                
329000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
329100     PERFORM IMS-STATUSKONTROLL                                           
329200     .                                                                    
329300     EJECT                                                                
329400                                                                          
329500 IMS-REPL-WLARTC25 SECTION.                                               
329600                                                                          
329700     MOVE '  ' TO GODK-STATUSKODER                                        
329800     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC25                     
329900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
330000     PERFORM IMS-STATUSKONTROLL                                           
330100     .                                                                    
330200     SKIP2                                                                
330300                                                                          
330400 IMS-ISRT-WLARTC25 SECTION.                                               
330500                                                                          
330600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
330700          DELIMITED BY SIZE INTO SSA1                                     
330800     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
330900          DELIMITED BY SIZE INTO SSA2                                     
331000     MOVE   'WLARTC25  '      TO SSA3                                     
331100     MOVE '  ' TO GODK-STATUSKODER                                        
331200     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-WLARTC25                     
331300                        SSA1 SSA2 SSA3                                    
331400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
331500     PERFORM IMS-STATUSKONTROLL                                           
331600     .                                                                    
331700     EJECT                                                                
331800 IMS-DLET-WLARTC25   SECTION.                                             
331900*WDK6                                                                     
332000     MOVE '  ' TO GODK-STATUSKODER                                        
332100     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-WLARTC25                     
332200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
332300     PERFORM IMS-STATUSKONTROLL                                           
332400     .                                                                    
332500     SKIP3                                                                
332600                                                                          
332700 IMS-GHU-WDK629 SECTION.                                                  
332800                                                                          
332900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
333000          DELIMITED BY SIZE INTO SSA1                                     
333100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
333200          DELIMITED BY SIZE INTO SSA2                                     
333300     STRING 'WDK629  (IDDCREF = ' W-IDDC-REF-X ')'                        
333400          DELIMITED BY SIZE INTO SSA3                                     
333500     MOVE '  GE' TO GODK-STATUSKODER                                      
333600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
333700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
333800     PERFORM IMS-STATUSKONTROLL                                           
333900     .                                                                    
334000     EJECT                                                                
334100 IMS-REPL-WDK629 SECTION.                                                 
334200                                                                          
334300     MOVE '  ' TO GODK-STATUSKODER                                        
334400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
334500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
334600     PERFORM IMS-STATUSKONTROLL                                           
334700     .                                                                    
334800     EJECT                                                                
334900 IMS-GET-UNIK-WLARTS01 SECTION.                                           
335000                                                                          
335100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
335200          DELIMITED BY SIZE INTO SSA1                                     
335300     MOVE '  GE' TO GODK-STATUSKODER                                      
335400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS01 SSA1                  
335500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
335600     PERFORM IMS-STATUSKONTROLL                                           
335700     .                                                                    
335800     SKIP2                                                                
335900                                                                          
336000 IMS-GET-UNIK-WLARTS11 SECTION.                                           
336100                                                                          
336200     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
336300          DELIMITED BY SIZE INTO SSA1                                     
336400     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
336500          DELIMITED BY SIZE INTO SSA2                                     
336600     MOVE '  GE' TO GODK-STATUSKODER                                      
336700     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
336800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
336900     PERFORM IMS-STATUSKONTROLL                                           
337000     .                                                                    
337100     EJECT                                                                
337200                                                                          
337300 IMS-GET-HOLD-UNIK-WLARTS11 SECTION.                                      
337400                                                                          
337500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
337600          DELIMITED BY SIZE INTO SSA1                                     
337700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
337800          DELIMITED BY SIZE INTO SSA2                                     
337900     MOVE '  GE' TO GODK-STATUSKODER                                      
338000     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
338100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
338200     PERFORM IMS-STATUSKONTROLL                                           
338300     .                                                                    
338400     EJECT                                                                
338500                                                                          
338600 IMS-GET-NEXT-WLARTS11 SECTION.                                           
338700                                                                          
338800     MOVE 'WLARTS11 '         TO SSA1                                     
338900     MOVE '  GE' TO GODK-STATUSKODER                                      
339000     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WLARTS11 SSA1                 
339100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
339200     PERFORM IMS-STATUSKONTROLL                                           
339300     .                                                                    
339400     SKIP2                                                                
339500                                                                          
339600 IMS-GET-NEXT-WLARTS11-2 SECTION.                                         
339700                                                                          
339800     STRING 'WLARTS11(IDDC     =' W-IDDC-X2 ')'                           
339900          DELIMITED BY SIZE INTO SSA1                                     
340000     MOVE '  GE' TO GODK-STATUSKODER                                      
340100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WLARTS11 SSA1                 
340200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
340300     PERFORM IMS-STATUSKONTROLL                                           
340400     .                                                                    
340500     SKIP2                                                                
340600                                                                          
340700 IMS-GET-HOLD-NEXT-WLARTS11 SECTION.                                      
340800                                                                          
340900     MOVE 'WLARTS11 '         TO SSA1                                     
341000     MOVE '  GE' TO GODK-STATUSKODER                                      
341100     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-WLARTS11 SSA1                
341200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
341300     PERFORM IMS-STATUSKONTROLL                                           
341400     .                                                                    
341500     EJECT                                                                
341600                                                                          
341700 IMS-REPL-WLARTS11 SECTION.                                               
341800                                                                          
341900     MOVE '  ' TO GODK-STATUSKODER                                        
342000     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WLARTS11                     
342100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
342200     PERFORM IMS-STATUSKONTROLL                                           
342300     .                                                                    
342400     EJECT                                                                
342500                                                                          
342600 IMS-GHU-WDK711 SECTION.                                                  
342700                                                                          
342800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
342900          DELIMITED BY SIZE INTO SSA1                                     
343000     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
343100          DELIMITED BY SIZE INTO SSA2                                     
343200     MOVE '  GE' TO GODK-STATUSKODER                                      
343300     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
343400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
343500     PERFORM IMS-STATUSKONTROLL                                           
343600     .                                                                    
343700     EJECT                                                                
343800                                                                          
343810 IMS-GHU-WDGX2404 SECTION.                                                
343830                                                                          
343840     STRING 'WDR501  (WDGXKEY  =' W-2403KEY-X ')'                         
343850          DELIMITED BY SIZE INTO SSA1                                     
343851     STRING 'WDGX2404(KY2404  =>' W-2404KEY-MIN-X                         
343852                    '&KY2404  =<' W-2404KEY-MAX-X')'                      
343863           DELIMITED BY SIZE INTO SSA2                                    
343864     MOVE '  GE' TO GODK-STATUSKODER                                      
343865     CALL CBLTDLI USING GHU 2404-PCB DLI-IO-WDGX2404 SSA1 SSA2            
343866     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
343867     PERFORM IMS-STATUSKONTROLL                                           
343868     .                                                                    
343869     EJECT                                                                
343892                                                                          
343893 IMS-REPL-2404 SECTION.                                                   
343894     MOVE '  ' TO GODK-STATUSKODER                                        
343895     CALL CBLTDLI USING REPL 2404-PCB DLI-IO-WDGX2404                     
343896     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
343897     PERFORM IMS-STATUSKONTROLL                                           
343898     .                                                                    
343899     EJECT                                                                
343900 IMS-DLET-2404 SECTION.                                                   
343901     MOVE '  ' TO GODK-STATUSKODER                                        
343902     CALL CBLTDLI USING DLET 2404-PCB DLI-IO-WDGX2404                     
343903     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
343904     PERFORM IMS-STATUSKONTROLL                                           
343905     .                                                                    
343906     EJECT                                                                
343907                                                                          
343910 IMS-GU-WDK711 SECTION.                                                   
344000                                                                          
344100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
344200          DELIMITED BY SIZE INTO SSA1                                     
344300     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
344400          DELIMITED BY SIZE INTO SSA2                                     
344500     MOVE '  GE' TO GODK-STATUSKODER                                      
344600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
344700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
344800     PERFORM IMS-STATUSKONTROLL                                           
344900     .                                                                    
345000     EJECT                                                                
345100                                                                          
345200 IMS-REPL-WDK711 SECTION.                                                 
345300                                                                          
345400     MOVE '  ' TO GODK-STATUSKODER                                        
345500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
345600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
345700     PERFORM IMS-STATUSKONTROLL                                           
345800     .                                                                    
345900     EJECT                                                                
346000                                                                          
346100 IMS-GET-BENA01-BSEQ    SECTION.                                          
346200                                                                          
346300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
346400            DELIMITED BY SIZE INTO SSA1                                   
346500     MOVE '  GE' TO GODK-STATUSKODER                                      
346600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1                  
346700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
346800     PERFORM IMS-STATUSKONTROLL                                           
346900     .                                                                    
347000     SKIP3                                                                
347100                                                                          
347200 IMS-GET-BENA11         SECTION.                                          
347300                                                                          
347400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
347500            DELIMITED BY SIZE INTO SSA1                                   
347600     MOVE '  ' TO GODK-STATUSKODER                                        
347700     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-WLBENA11 SSA1                 
347800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
347900     PERFORM IMS-STATUSKONTROLL                                           
348000     .                                                                    
348100     EJECT                                                                
348200 IMS-ISRT-4505-WL450511 SECTION.                                          
348300                                                                          
348400     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
348500            DELIMITED BY SIZE INTO SSA1                                   
348600     MOVE 'WL450511 '           TO SSA2                                   
348700     MOVE '  '   TO GODK-STATUSKODER                                      
348800     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-WL450511 SSA1 SSA2           
348900     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
349000     PERFORM IMS-STATUSKONTROLL                                           
349100     .                                                                    
349200     SKIP3                                                                
349300 IMS-GET-W6D201 SECTION.                                                  
349400     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
349500             DELIMITED BY SIZE INTO SSA1                                  
349600     MOVE '  GE' TO GODK-STATUSKODER                                      
349700     CALL CBLTDLI USING GU W6D2-PCB DLI-IO-W6D201 SSA1                    
349800     MOVE W6D2-STATUS-CODE TO STATUS-WS                                   
349900     PERFORM IMS-STATUSKONTROLL                                           
350000     .                                                                    
350100     SKIP3                                                                
350200 IMS-GET-W6D211 SECTION.                                                  
350300     MOVE 'W6D211  ' TO SSA1                                              
350400     MOVE '  GE' TO GODK-STATUSKODER                                      
350500     CALL CBLTDLI USING GNP W6D2-PCB DLI-IO-W6D211 SSA1                   
350600     MOVE W6D2-STATUS-CODE TO STATUS-WS                                   
350700     PERFORM IMS-STATUSKONTROLL                                           
350800     .                                                                    
350900     EJECT                                                                
351000 IMS-GU-WDP311 SECTION.                                                   
351100     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
351200          DELIMITED BY SIZE INTO SSA1                                     
351300     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
351400          DELIMITED BY SIZE INTO SSA2                                     
351500     MOVE '  GE' TO GOdk-STATUSkoder                                      
351600     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
351700     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
351800     PERFORM IMS-STATUSKONTROLL                                           
351900     .                                                                    
352000     SKIP2                                                                
352100 IMS-GU-WDP3A SECTION.                                                    
352200     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
352300                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
352400                    '&KDARBTYP =' W-KDARBTYP ')'                          
352500            DELIMITED BY SIZE INTO SSA1                                   
352600     MOVE '  GE' TO GOdk-STATUSkoder                                      
352700     CALL CBLTDLI USING GU WDP3A-PCB DLI-IO-WDP3A SSA1                    
352800     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
352900     PERFORM IMS-STATUSKONTROLL                                           
353000     .                                                                    
353100     SKIP2                                                                
353200 IMS-GN-WDP3A SECTION.                                                    
353300     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
353400                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
353500                    '&KDARBTYP =' W-KDARBTYP ')'                          
353600            DELIMITED BY SIZE INTO SSA1                                   
353700     MOVE '  GEGB' TO GOdk-STATUSkoder                                    
353800     CALL CBLTDLI USING GN WDP3A-PCB DLI-IO-WDP3A SSA1                    
353900     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
354000     PERFORM IMS-STATUSKONTROLL                                           
354100     .                                                                    
354200     EJECT                                                                
354300 IMS-GU-WDP3B SECTION.                                                    
354400     STRING 'WDP3B1  (WDP3B1KY =' W-WDP3B1-X ')'                          
354500            DELIMITED BY SIZE INTO SSA1                                   
354600     MOVE '  GE' TO GOdk-STATUSKODER                                      
354700     CALL CBLTDLI USING GU WDP3B-PCB DLI-IO-AREA-WDP3B SSA1               
354800     MOVE WDP3B-STATUS-CODE TO STATUS-WS                                  
354900     PERFORM IMS-STATUSKONTROLL                                           
355000     .                                                                    
355100     SKIP2                                                                
355200 IMS-GU-WDP3C SECTION.                                                    
355300     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
355400                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
355500                    '&KDARBTYP =' W-KDARBTYP ')'                          
355600            DELIMITED BY SIZE INTO SSA1                                   
355700     MOVE '  GE' TO GOdk-STATUSKODER                                      
355800     CALL CBLTDLI USING GU WDP3C-PCB DLI-IO-AREA-WDP3C SSA1               
355900     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
356000     PERFORM IMS-STATUSKONTROLL                                           
356100     .                                                                    
356200     eject                                                                
356300 IMS-GN-WDP3C SECTION.                                                    
356400     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
356500                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
356600                    '&KDARBTYP =' W-KDARBTYP ')'                          
356700            DELIMITED BY SIZE INTO SSA1                                   
356800     MOVE '  GEGB' TO GOdk-STATUSKODER                                    
356900     CALL CBLTDLI USING GN WDP3C-PCB DLI-IO-AREA-WDP3C SSA1               
357000     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
357100     PERFORM IMS-STATUSKONTROLL                                           
357200     .                                                                    
357300     Eject                                                                
357400 IMS-GU-WDB601    SECTION.                                                
357500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
357600          DELIMITED BY SIZE INTO SSA1                                     
357700     MOVE '  GE' TO GODK-STATUSKODER                                      
357800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
357900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
358000     PERFORM IMS-STATUSKONTROLL                                           
358100     .                                                                    
358200     EJECT                                                                
358300 IMS-GU-WDGX6331 SECTION.                                                 
358400                                                                          
358500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
358600          DELIMITED BY SIZE INTO SSA1                                     
358700     MOVE '  GE' TO GODK-STATUSKODER                                      
358800     CALL CBLTDLI USING GU 6331-PCB DLI-IO-WDGX6331 SSA1                  
358900     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
359000     PERFORM IMS-STATUSKONTROLL                                           
359100     .                                                                    
359200     EJECT                                                                
359300 IMS-GU-WDGX6332 SECTION.                                                 
359400                                                                          
359500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
359600          DELIMITED BY SIZE INTO SSA1                                     
359700     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
359800          DELIMITED BY SIZE INTO SSA2                                     
359900     MOVE '  GE' TO GODK-STATUSKODER                                      
360000     CALL CBLTDLI USING GU 6331-PCB DLI-IO-WDGX6332 SSA1 SSA2             
360100     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
360200     PERFORM IMS-STATUSKONTROLL                                           
360300     .                                                                    
360400     SKIP3                                                                
360500 IMS-GNP-WDGX6332 SECTION.                                                
360600                                                                          
360700     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
360800          DELIMITED BY SIZE INTO SSA1                                     
360900     MOVE '  GE' TO GODK-STATUSKODER                                      
361000     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6332 SSA1                 
361100     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
361200     PERFORM IMS-STATUSKONTROLL                                           
361300     .                                                                    
361400     SKIP3                                                                
361500 IMS-GU-WDGX6334 SECTION.                                                 
361600                                                                          
361700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
361800          DELIMITED BY SIZE INTO SSA1                                     
361900     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
362000          DELIMITED BY SIZE INTO SSA2                                     
362100     STRING 'WDGX6334(IDDC     =' W-IDDC-6334 ')'                         
362200          DELIMITED BY SIZE INTO SSA3                                     
362300     MOVE '  GE' TO GODK-STATUSKODER                                      
362400     CALL CBLTDLI USING GU 6331-PCB DLI-IO-WDGX6334 SSA1 SSA2 SSA3        
362500     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
362600     PERFORM IMS-STATUSKONTROLL                                           
362700     .                                                                    
362800     SKIP3                                                                
362900 IMS-GNP-WDGX6332-2 SECTION.                                              
363000                                                                          
363100     MOVE 'WDGX6332 ' TO SSA1                                             
363200     MOVE '  GE' TO GODK-STATUSKODER                                      
363300     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6332 SSA1                 
363400     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
363500     PERFORM IMS-STATUSKONTROLL                                           
363600     .                                                                    
363700     SKIP3                                                                
363800 IMS-GNP-WDGX6334 SECTION.                                                
363900                                                                          
364000     MOVE 'WDGX6334 ' TO SSA1                                             
364100     MOVE '  GE' TO GODK-STATUSKODER                                      
364200     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6334 SSA1                 
364300     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
364400     PERFORM IMS-STATUSKONTROLL                                           
364500     .                                                                    
364600     SKIP3                                                                
364700 IMS-GNP-WDGX6334-2 SECTION.                                              
364800                                                                          
364900     STRING 'WDGX6334(IDDC     =' W-IDDC-6334 ')'                         
365000          DELIMITED BY SIZE INTO SSA1                                     
365100     MOVE '  GE' TO GODK-STATUSKODER                                      
365200     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6334 SSA1                 
365300     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
365400     PERFORM IMS-STATUSKONTROLL                                           
365500     .                                                                    
365600     SKIP3                                                                
365700 IMS-GNP-WDGX6334-32 SECTION.                                             
365800                                                                          
365900     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
366000          DELIMITED BY SIZE INTO SSA1                                     
366100     MOVE 'WDGX6334 ' TO SSA2                                             
366200     MOVE '  GE' TO GODK-STATUSKODER                                      
366300     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6334 SSA1 SSA2            
366400     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
366500     PERFORM IMS-STATUSKONTROLL                                           
366600     .                                                                    
366700     SKIP3                                                                
366800 IMS-STATUSKONTROLL SECTION.                                              
366900                                                                          
367000     SET STATUS-IX TO 1                                                   
367100     SEARCH GODK-STATUS                                                   
367200       AT END                                                             
367300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
367400         DELIMITED BY SIZE INTO FELTEXT                                   
367500         CALL FELLOG                                                      
367600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
367700         CONTINUE                                                         
367800     END-SEARCH                                                           
368000     .                                                                    
