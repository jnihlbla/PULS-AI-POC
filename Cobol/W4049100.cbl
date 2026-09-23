001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4049100.                                                
001600 AUTHOR.         MOGREN STINA.                                            
001700 DATE-WRITTEN.   06/08/18.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        PROGRAMMET ÄR EN BAKGRUNDS-MPP FÖR                               
002110*        UPPDATERING AV LANDBENÄMNINGS-TABELL (BELAND)                    
002200*        STARTAS AV ÖVERFÖRING FRÅN BILL-IT GENOM WZ01-RUTINEN            
002300*        INFORMATIONEN KOMMER PER IDLANDX3                                
002420*                                                                         
002500*        WDR5 UPPDATERAS                                                  
002510*        HELA POSTEN ERSÄTTS                                              
002511*                                                                         
002801*        PROGRAMMET UPPDATERAR WDR5 (LANDBENÄMNINGS-TABELL)               
002900*                                                                         
003000*    INDATA.                                                              
003100*        TRANSAKTION: W4T491X                                             
003300*                                                                         
003400*    UTDATA.                                                              
003600                                                                          
003700     SKIP2                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'W4049100'.            
004400                                                                          
004500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004700                                                                          
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005140                                                                          
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400                                                                          
005501 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005502     88  INDATA-OK                           VALUE 'J'.                   
005510     88  INDATA-FEL                          VALUE 'N'.                   
005600                                                                          
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
005910                                                                          
006010 77  SKRIV-POST-SW               PIC X       VALUE 'N'.                   
006020     88  SKRIV-POST                          VALUE 'J'.                   
006030     88  SKRIV-POST-NEJ                      VALUE 'N'.                   
006040                                                                          
006100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006200     88  EGEN-MID                            VALUE '4491'.                
006300     88  GODK-MID                            VALUE '4491'.                
006800     88  HELP-MID                            VALUE '0551'.                
006900     EJECT                                                                
006901 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
006902                                                                          
006903 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
006904 77  IXA                         PIC S9(3)   VALUE ZERO COMP-3.           
006905                                                                          
006909 77  DUMMY-AREA                  PIC X(1)    VALUE SPACE.                 
006914                                                                          
006915*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
006916 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
006919                                                                          
006927 01  WS-TIKLOCK                  PIC S9(9)   VALUE ZERO COMP-3.           
006929 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006931                                                                          
006932 01  WS-DCUSER.                                                           
006933     03  FILLER                   PIC X(5)   VALUE 'WIDDC'.               
006934     03  WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                 
006935     03  FILLER                   PIC X(1)   VALUE SPACE.                 
006936                                                                          
006937 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
006938 01  WS-SEKTION                  PIC X(32)   VALUE SPACE.                 
006939                                                                          
006945 01  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
006948                                                                          
006949 01  FILLER                      PIC X(16)   VALUE 'WS-IDLANDX3'.         
006959 01  WS-IDLANDX3                 PIC X(3)    VALUE SPACE.                 
006960 01  WS-BELAND                   PIC X(35)   VALUE SPACE.                 
006964 01  WS-DAUPPDAT                 PIC X(8)    VALUE SPACE.                 
006966                                                                          
006967 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
006980*                                                                         
006997                                                                          
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007620     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
007700     EJECT                                                                
007725*                                                                         
007800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007900*01 -COPY WMEDAREA                                                        
008501     EJECT                                                                
008510*01  -COPY WDATAREA                                                       
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009000     SKIP3                                                                
009100*01 -COPY WMSGINIT                                                        
009200     EJECT                                                                
009300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009400*                                                                         
009500 01  SPAR-AREA.                                                           
009600     03  SPAR-IDTRANS            PIC X(4)    VALUE '4491'.                
009800     EJECT                                                                
009801 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
009802                                                                          
009805 01  FILLER                      PIC X(16)   VALUE 'WMSGSOP '.            
009806 01   PROG-TO-PROG-SW.                                                    
009807     03  -COPY WMSGSOP                                                    
009808                                                                          
009813*    --- AREOR FÖR ANROP FRÅN  WZ01                                       
009820 01  FILLER                      PIC X(16)   VALUE 'WZ01-RECV '.          
009830*01  -COPY WZ01RECV                                                       
009840                                                                          
009841 01  FILLER                      PIC X(16)   VALUE 'WF1017    '.          
009842*                WF101700  FEEDBACK TO PULS                               
009843 01  INPOST.                                                              
009844*03  -COPY WZ01REQU   -PRE IN-                                            
009851 03  WF-AREA.                                                             
009852     05  WF-BELAND             PIC X(35)   VALUE SPACE.                   
009853     05  WF-IDLANDX3           PIC X(3)    VALUE SPACE.                   
009860*                                                                         
009870 01  RECV-DATA             REDEFINES INPOST.                              
009880     03  FILLER            PIC X(500).                                    
009881                                                                          
009893                                                                          
009903                                                                          
011600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  NYCKLAR-TILL-DLI.                                                    
012120                                                                          
012127     03  W-KY4138-X.                                                      
012128         05  W-IDLANDX3          PIC X(3)    VALUE SPACE.                 
012137     03  W-KDSEGKEY-X.                                                    
012138         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
012190     03  W-WDGXKEY-4137-X.                                                
012191         05  W-IDHTYP-4137       PIC X(4)    VALUE '4137'.                
012192         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
012199                                                                          
012210     SKIP2                                                                
012300*    --- STATUS-KOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FINNS                       VALUE '  '.                  
012600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012710     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012720     88  IMS-EJ-OK                           VALUE 'XD'.                  
012800     SKIP2                                                                
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013400     EJECT                                                                
013500*    --- IMS FUNKTIONSKODER                                               
013600*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000                                                                          
014320 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDGX4138'.               
014330 01  DLI-IO-WDGX4138.                                                     
014340*    03   -COPY WDGX4138                                                  
014380                                                                          
014381 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDR501'.                 
014382 01  DLI-IO-WDR501.                                                       
014383*    03   -COPY WDGX01                                                    
014384                                                                          
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600 01  IO-PCB                      PIC X.                                   
014610                                                                          
014902*01  -COPY W0009  -PRE COUN-                                              
014903                                                                          
014907*01  -COPY W0008  -PRE WDP7-                                              
014908     05  FILLER                  PIC X.                                   
014909                                                                          
015103*01  -COPY W0008  -PRE 4138-                                              
015104     05  FILLER                  PIC X.                                   
015105     EJECT                                                                
015109 PROCEDURE DIVISION  USING  IO-PCB                                        
015110                           COUN-PCB                                       
015112                           WDP7-PCB                                       
015120                           4138-PCB.                                      
015121 MAIN SECTION.                                                            
015122     ENTRY 'DLITCBL' USING  IO-PCB                                        
015123                           COUN-PCB                                       
015125                           WDP7-PCB                                       
015140                           4138-PCB.                                      
015200                                                                          
015600     PERFORM A-INIT                                                       
015810     PERFORM S01-RECV-OPEN                                                
015820     PERFORM S02-RECV-MESSAGE                                             
015830     PERFORM UNTIL RECV-KDRC > 0                                          
015841                                                                          
015845       PERFORM D-NYCKLAR-OSV                                              
015846       PERFORM IMS-GHU-WDGX4138                                           
015853       IF SEGMENT-FINNS                                                   
015864                                                                          
015866             PERFORM E-UPPDAT                                             
015867             PERFORM IMS-REPL-WDGX4138                                    
015868                                                                          
015871       ELSE                                                               
015881             PERFORM E-UPPDAT                                             
015882             PERFORM IMS-ISRT-WDGX4138                                    
015883                                                                          
015888       END-IF                                                             
015891       PERFORM S02-RECV-MESSAGE                                           
015899     END-PERFORM                                                          
015900     IF RECV-KDRC > 1                                                     
015901       MOVE 'WZ01-RECV AVSLUTAS FEL' TO FELTEXT                           
015902       CALL FELLOG                                                        
015910     END-IF                                                               
016000     PERFORM S03-RECV-CLOSE                                               
017100                                                                          
017110     PERFORM Z-FINIT                                                      
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017610     MOVE 'A-INIT'               TO WS-SEKTION                            
017700                                                                          
017710     ACCEPT DAGENS-DATUM      FROM DATE                                   
017720     MOVE DAGENS-DATUM        TO WS-DAUPPDAT(3:6)                         
017730     MOVE '20'                TO WS-DAUPPDAT(1:2)                         
017800     ACCEPT WS-TIKLOCK        FROM TIME                                   
017910                                                                          
017927     MOVE SPACE            TO STATUS-WS                                   
017930     PERFORM IMS-GHU-WDR5-4137                                            
017940     IF SEGMENT-SAKNAS                                                    
017941       MOVE SPACE    TO WDGX01                                            
017942       MOVE '4137'   TO IDHTYP                                            
017950       PERFORM IMS-ISRT-WDR5-4137                                         
017951     ELSE                                                                 
017952       PERFORM IMS-GHNP-WDGX4138                                          
017953       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
017954         PERFORM IMS-DEL-WDGX4138                                         
017955         PERFORM IMS-GHNP-WDGX4138                                        
017956       END-PERFORM                                                        
017960     END-IF                                                               
020500     .                                                                    
020600     EJECT                                                                
024300 D-NYCKLAR-OSV  SECTION.                                                  
024400     MOVE 'D-NYCKLAR-OSV '      TO WS-SEKTION                             
024500                                                                          
024600     MOVE SPACE                 TO WS-IDLANDX3                            
024601                                   WS-BELAND                              
024700     MOVE WF-IDLANDX3               TO WS-IDLANDX3                        
024701     MOVE WS-IDLANDX3               TO W-IDLANDX3                         
024702                                                                          
024707     MOVE WF-BELAND                 TO WS-BELAND                          
024709                                                                          
025310     .                                                                    
025400     EJECT                                                                
025500 E-UPPDAT  SECTION.                                                       
025600     MOVE 'E-UPPDAT '            TO WS-SEKTION                            
025700                                                                          
025720     MOVE WS-IDLANDX3            TO 4138-IDLANDX3                         
025792     MOVE WS-BELAND              TO 4138-BELAND                           
025795     MOVE WS-DAUPPDAT            TO 4138-DAUPPDAT                         
025810     .                                                                    
025900     EJECT                                                                
027117                                                                          
027208 Z-FINIT  SECTION.                                                        
027210     MOVE 'Z-FINIT'              TO WS-SEKTION                            
027300                                                                          
027800     .                                                                    
027900     EJECT                                                                
028700 S01-RECV-OPEN SECTION.                                                   
028710     MOVE 'S01-RECV-OPEN'        TO WS-SEKTION                            
028720                                                                          
028800     MOVE 'OPEN'                   TO RECV-KDFUNC                         
029000     MOVE 'CARPARTS.PULS.RECCOUNTRY' TO RECV-ADDISPABS                    
029100                                                                          
029200     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
029300                                   RECV-OPEN-AREA                         
029400                                                                          
029500     IF RECV-KDRC > 0                                                     
029600      MOVE RECV-KDRC               TO KDRC-DISP                           
029700      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
029800        DELIMITED BY SIZE INTO FELTEXT                                    
030000      CALL FELLOG                                                         
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 S02-RECV-MESSAGE SECTION.                                                
030401     MOVE 'S02-RECV-MESSAGE'     TO WS-SEKTION                            
030402                                                                          
030410     MOVE 'GET'                  TO RECV-KDFUNC                           
030420     MOVE LENGTH OF RECV-DATA    TO RECV-KVDLEN                           
030430     CALL WZ01RECV USING RECV-CONTROL-AREA                                
030440                         RECV-KVDLEN                                      
030450                         RECV-DATA                                        
030460*                                                                         
030470     IF RECV-KDRC > 1                                                     
030480       MOVE RECV-KDRC            TO KDRC-DISP                             
030490       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
030491         DELIMITED BY SIZE INTO FELTEXT                                   
030493       CALL FELLOG                                                        
030494     END-IF                                                               
030495     .                                                                    
030496     EJECT                                                                
030497 S03-RECV-CLOSE SECTION.                                                  
030498     MOVE 'S03-RECV-CLOSE'       TO WS-SEKTION                            
030499                                                                          
030500     MOVE 'CLOSE'                TO RECV-KDFUNC                           
030501     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
030510*                                                                         
030520     IF RECV-KDRC > 0                                                     
030530       MOVE RECV-KDRC            TO KDRC-DISP                             
030540       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
030550         DELIMITED BY SIZE INTO FELTEXT                                   
030570       CALL FELLOG                                                        
030580     END-IF                                                               
030590     .                                                                    
030591     EJECT                                                                
031012                                                                          
031013* --- IMS SEKTIONER ---                                                   
031020     SKIP3                                                                
033535 IMS-GHU-WDR5-4137 SECTION.                                               
033536     MOVE 'IMS-GHU-WDR5-4137'   TO WS-SEKTION                             
033537                                                                          
033538     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4137-X ')'                    
033539          DELIMITED BY SIZE INTO SSA1                                     
033540     MOVE '  GE' TO GODK-STATUSKODER                                      
033541     CALL CBLTDLI USING GHU 4138-PCB DLI-IO-WDR501 SSA1                   
033550     MOVE 4138-STATUS-CODE TO STATUS-WS                                   
033551     PERFORM IMS-STATUSKONTROLL                                           
033552     .                                                                    
033553     SKIP3                                                                
033554 IMS-ISRT-WDR5-4137 SECTION.                                              
033555     MOVE 'IMS-ISRT-WDGX4137'    TO WS-SEKTION                            
033556                                                                          
033557     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4137-X ')'                    
033558          DELIMITED BY SIZE INTO SSA1                                     
033562     MOVE '  II' TO GODK-STATUSKODER                                      
033563     CALL CBLTDLI USING ISRT 4138-PCB DLI-IO-WDR501 SSA1                  
033564     MOVE 4138-STATUS-CODE TO STATUS-WS                                   
033565     PERFORM IMS-STATUSKONTROLL                                           
033566     .                                                                    
033567     EJECT                                                                
033568 IMS-ISRT-WDGX4138 SECTION.                                               
033569     MOVE 'IMS-ISRT-WDGX4138'    TO WS-SEKTION                            
033570                                                                          
033571     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4137-X ')'                    
033572          DELIMITED BY SIZE INTO SSA1                                     
033573     MOVE 'WDGX4138  '          TO SSA2                                   
033574     MOVE '  II' TO GODK-STATUSKODER                                      
033575     CALL CBLTDLI USING ISRT 4138-PCB DLI-IO-WDGX4138 SSA1 SSA2           
033576     MOVE 4138-STATUS-CODE TO STATUS-WS                                   
033577     PERFORM IMS-STATUSKONTROLL                                           
033578     .                                                                    
033579     EJECT                                                                
033580 IMS-GHU-WDGX4138 SECTION.                                                
033581     MOVE 'IMS-GHU-WDGX4138'    TO WS-SEKTION                             
033582                                                                          
033583     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4137-X ')'                    
033584          DELIMITED BY SIZE INTO SSA1                                     
033585     STRING 'WDGX4138*F(IDLANDX3 =' W-KY4138-X ')'                        
033586          DELIMITED BY SIZE INTO SSA2                                     
033587     MOVE '  GE' TO GODK-STATUSKODER                                      
033588     CALL CBLTDLI USING GHU 4138-PCB DLI-IO-WDGX4138 SSA1 SSA2            
033589     MOVE 4138-STATUS-CODE TO STATUS-WS                                   
033590     PERFORM IMS-STATUSKONTROLL                                           
033591     .                                                                    
033592     EJECT                                                                
033593 IMS-GHNP-WDGX4138 SECTION.                                               
033594     MOVE 'IMS-GHNP-WDGX4138'    TO WS-SEKTION                            
033595                                                                          
033596     MOVE    'WDGX4138'          TO    SSA1                               
033597     MOVE '  GEGB' TO GODK-STATUSKODER                                    
033598     CALL CBLTDLI USING GHNP 4138-PCB DLI-IO-WDGX4138 SSA1                
033599     MOVE 4138-STATUS-CODE       TO STATUS-WS                             
033600     PERFORM IMS-STATUSKONTROLL                                           
033601     .                                                                    
033602     SKIP3                                                                
033603 IMS-REPL-WDGX4138 SECTION.                                               
033604     MOVE 'IMS-REPL-WDGX4138'    TO WS-SEKTION                            
033605                                                                          
033606     MOVE '    ' TO GODK-STATUSKODER                                      
033607     CALL CBLTDLI USING REPL 4138-PCB DLI-IO-WDGX4138                     
033608     MOVE 4138-STATUS-CODE       TO STATUS-WS                             
033609     PERFORM IMS-STATUSKONTROLL                                           
033610     .                                                                    
033611     EJECT                                                                
033620 IMS-DEL-WDGX4138 SECTION.                                                
033621     MOVE 'IMS-DEL-WDGX4138'    TO WS-SEKTION                             
033622                                                                          
033623     MOVE '    ' TO GODK-STATUSKODER                                      
033624     CALL CBLTDLI USING DLET 4138-PCB DLI-IO-WDGX4138                     
033625     MOVE 4138-STATUS-CODE TO STATUS-WS                                   
033626     PERFORM IMS-STATUSKONTROLL                                           
033627     .                                                                    
033628     EJECT                                                                
033629 IMS-STATUSKONTROLL SECTION.                                              
033630                                                                          
033631     SET STATUS-IX TO 1                                                   
033632     SEARCH GODK-STATUS                                                   
033640       AT END                                                             
033700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033800         DELIMITED BY SIZE INTO FELTEXT                                   
033900         CALL FELLOG                                                      
034000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034100         CONTINUE                                                         
034200     END-SEARCH                                                           
034300     .                                                                    
034400     SKIP2                                                                
