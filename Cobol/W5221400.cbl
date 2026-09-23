020000 ID DIVISION.                                                             
030020 PROGRAM-ID.     W5221400.                                                
040000 AUTHOR.         NILSSON LINDA.                                           
050000 DATE-WRITTEN.   03/02/20.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*                                                                         
080102*    FUNCTION:                                                            
081020* ------ ANVÄND -CTX W52211 TILL INTRASTAT I PGM W5221400                 
082001* ------ NYTT PGM SOM BEHANDLAR INFILEN W52213 FRÅN W5221100              
083001*                                                                         
130000*    ABENDCODES:                                                          
140000*        U0016 -  . . . .                                                 
150000*        U1000 -  . . . .                                                 
160000*                                                                         
170000                                                                          
180000     SKIP3                                                                
190000 ENVIRONMENT DIVISION.                                                    
200000     SKIP2                                                                
210000 INPUT-OUTPUT SECTION.                                                    
220000                                                                          
230000 FILE-CONTROL.                                                            
240100     SKIP2                                                                
240206*          --- INTRASTAT TILL ON DEMAND - INFIL                           
240320     SELECT W52213                     ASSIGN TO W52214D1.                
240400     SKIP2                                                                
240806*          --- INTRASTAT TILL ON DEMAND - LISTA                           
240920     SELECT W52214-001                 ASSIGN TO W52214D2.                
241000     SKIP2                                                                
270000 DATA DIVISION.                                                           
280000     SKIP3                                                                
290000 FILE SECTION.                                                            
300100     SKIP3                                                                
300204 FD  W52213                                                               
300300     RECORDING       F                                                    
300400     BLOCK CONTAINS  0.                                                   
300500                                                                          
300600*01  -COPY W52211      -L.                                                
300700     SKIP3                                                                
301420 FD  W52214-001                                                           
301500     RECORDING       F                                                    
301600     BLOCK CONTAINS  0.                                                   
301700     SKIP2                                                                
301846 01  W52214-001-LINE             PIC X(155).                              
310000     EJECT                                                                
320000 WORKING-STORAGE SECTION.                                                 
330000                                                                          
340020 77  IDPGM                       PIC X(8)    VALUE 'W5221400'.            
350000 77  YES                         PIC X       VALUE 'J'.                   
360000 77  NOO                         PIC X       VALUE 'N'.                   
380100                                                                          
380220 77  W52213-EOF-SW               PIC X       VALUE 'N'.                   
380307     88  END-OF-W52213                       VALUE 'Y'.                   
380407     EJECT                                                                
400000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
410000 01  FILLER REDEFINES TODAYS-DATE.                                        
420000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
430000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
440000     03  TODAYS-DATE-DAY         PIC 9(2).                                
450000     EJECT                                                                
460000 01  GENERAL-SUBPROGRAMS.                                                 
470000*                                                                         
480000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
491000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
500000     SKIP2                                                                
501051*    --- SPAR-AREA FÖR RADBRYTNING                                        
502051 01  WS-SPAR.                                                             
503051     03  SPAR-DAFINDOC           PIC 9(8)  VALUE ZERO.                    
504051     03  SPAR-IDFINDOC           PIC 9(9)  VALUE ZERO.                    
510000*    --- PARAMETERS TO ABEND                                              
520000                                                                          
530000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
540000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
550000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
560000     SKIP2                                                                
570000 01  ERRTEXT.                                                             
580000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
590000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
600100     EJECT                                                                
600200*    --- PARAMETRAR TILL POSTSUM                                          
600300*                                                                         
601000*01  -COPY W0005   -PRE  POSTSUM-                                         
620100     EJECT                                                                
620810 01  W52213-AREA-START              PIC X(24)   VALUE                     
620910                                 'W52213-AREA-START  '.                   
621000     SKIP2                                                                
621110 01  W52213-AREA.                                                         
621215*    03  -COPY W52211     -PRE INT-                                       
621300     EJECT                                                                
621400 01  W001-AREA-START             PIC X(24)   VALUE                        
621500                                 'W001-AREA-START  '.                     
621600     SKIP2                                                                
621700 01  W001-HELPAREAS.                                                      
621800*                                                                         
621900     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
622220     03  W001-LISTNR             PIC X(11)   VALUE 'W52214-001'.          
622300     03  W001-PAGECOUNTER        PIC S9(5)   COMP-3 VALUE ZERO.           
622500     EJECT                                                                
622632 01  W001-LINE.                                                           
622839     03  FILLER                  PIC X     VALUE SPACE.                   
622939     03  W001-TIAA               PIC 9(2)  VALUE ZERO.                    
623024     03  W001-TIMM               PIC 9(2)  VALUE ZERO.                    
624034     03  FILLER                  PIC X     VALUE SPACE.                   
625024     03  W001-IDLANDX3-SEND      PIC X(3)  VALUE SPACE.                   
626034     03  FILLER                  PIC X     VALUE SPACE.                   
626124     03  W001-IDLANDX3-REC       PIC X(3)  VALUE SPACE.                   
626334     03  FILLER                  PIC X     VALUE SPACE.                   
626424     03  W001-KDVALISO           PIC X(3)  VALUE SPACE.                   
626948     03  FILLER                  PIC X(2)  VALUE SPACE.                   
627024     03  W001-KDINTTYP-OLD       PIC 9     VALUE ZERO.                    
628048     03  FILLER                  PIC X     VALUE SPACE.                   
628132     03  W001-DAFINDOC           PIC 9(8)  VALUE ZERO.                    
628234     03  FILLER                  PIC X     VALUE SPACE.                   
628324     03  W001-IDFINDOC           PIC 9(9)  VALUE ZERO.                    
628534     03  FILLER                  PIC X     VALUE SPACE.                   
628624     03  W001-IDSTATNR           PIC 9(8)  VALUE ZERO.                    
628824     03  FILLER                  PIC X     VALUE SPACE.                   
628926     03  W001-IDARTNR            PIC 9(9)  VALUE ZERO.                    
629124     03  FILLER                  PIC X     VALUE SPACE.                   
629236     03  W001-BEART              PIC X(15) VALUE SPACE.                   
629434     03  FILLER                  PIC X     VALUE SPACE.                   
629568     03  W001-VKORDNTO           PIC 9(5)V9(1) VALUE ZERO.                
629734     03  FILLER                  PIC X     VALUE SPACE.                   
629824     03  W001-KVLEVART           PIC 9(7)  VALUE ZERO.                    
630034     03  FILLER                  PIC X(2)  VALUE SPACE.                   
630124     03  W001-KDARTURS           PIC X(2)  VALUE SPACE.                   
630734     03  FILLER                  PIC X     VALUE SPACE.                   
630868     03  W001-SUNTO              PIC +9(10)V9(2) VALUE ZERO.              
631234     03  FILLER                  PIC X     VALUE SPACE.                   
631334     03  W001-KDBEH              PIC 9(2)  VALUE ZERO.                    
631534     03  FILLER                  PIC X     VALUE SPACE.                   
631624     03  W001-IDDISTR            PIC 9(4)  VALUE ZERO.                    
631724     03  FILLER                  PIC X     VALUE SPACE.                   
631824     03  W001-IDKUNDNR           PIC 9(6)  VALUE ZERO.                    
631825     03  FILLER                  PIC X     VALUE SPACE.                   
631826     03  W001-IDVAT              PIC X(17) VALUE SPACE.                   
631827     03  FILLER                  PIC X     VALUE SPACE.                   
6A1828     03  W001-KDINTTYP           PIC 9(2)  VALUE ZERO.                    
 29734     03  FILLER                  PIC X     VALUE SPACE.                   
629568     03  W001-VKORDNTO-3DEC      PIC 9(5)V9(3) VALUE ZERO.                
629734     03  FILLER                  PIC X     VALUE SPACE.                   
632200     EJECT                                                                
632300 01  W001-HEADER1.                                                        
632400*                                                                         
632500     03  FILLER                  PIC X(3).                                
632616     03  FILLER                  PIC X(27) VALUE                          
632716                                    'VOLVO CAR CUSTOMER SERVICE '.        
632825     03  FILLER                  PIC X(65) VALUE                          
632925                                       '  INTRASTAT       '.              
633049     03  FILLER                  PIC X(11) VALUE SPACE.                   
633238     03  FILLER                  PIC X(6) VALUE ' PAGE '.                 
633300     03  W001-PAGE               PIC Z(4)9.                               
633400     SKIP2                                                                
633532 01  W001-HEADER2.                                                        
633636     03  FILLER                  PIC X(4)  VALUE ' PER'.                  
633734     03  FILLER                  PIC X(4)  VALUE ' SND'.                  
633841     03  FILLER                  PIC X(5)  VALUE ' REC '.                 
634047     03  FILLER                  PIC X(4)  VALUE ' CUR'.                  
634340     03  FILLER                  PIC X(3)  VALUE ' TC'.                   
634642     03  FILLER                  PIC X(9)  VALUE ' DOC.DATE'.             
634742     03  FILLER                  PIC X(10) VALUE '    DOC.ID'.            
635134     03  FILLER                  PIC X(9)  VALUE '  STATNO.'.             
635336     03  FILLER                  PIC X(26) VALUE                          
635467                                     '          PART DESCRIPTION'.        
635534     03  FILLER                  PIC X(7)  VALUE ' WEIGHT'.               
635634     03  FILLER                  PIC X(8)  VALUE '  Q. DEL'.              
635834     03  FILLER                  PIC X(4)  VALUE ' ORG'.                  
636334                                                                          
636434     03  FILLER                  PIC X(14) VALUE '    NET AMOUNT'.        
636834     03  FILLER                  PIC X(3)  VALUE ' FC'.                   
637034     03  FILLER                  PIC X(5)  VALUE ' DIST'.                 
637134     03  FILLER                  PIC X(7)  VALUE '   CUST'.               
637135     03  FILLER                  PIC X(17)                                
637136                                 VALUE '     IDVAT       '.               
637334     03  FILLER                  PIC X(3)  VALUE ' TC'.                   
635534     03  FILLER                  PIC X(11)  VALUE '  WEIGHT-GM'.          
637446     03  FILLER                  PIC X(07) VALUE SPACE.                   
637534                                                                          
637634     EJECT                                                                
637734                                                                          
638023     EJECT                                                                
640010 PROCEDURE DIVISION.                                                      
650010 MAIN SECTION.                                                            
670000     SKIP2                                                                
680000                                                                          
690000     PERFORM A-INIT                                                       
700120     PERFORM S01-READ-W52213                                              
710010     PERFORM UNTIL END-OF-W52213                                          
721039       MOVE INT-TIAAAA (3:2)  TO W001-TIAA                                
730012       MOVE INT-TIMM          TO W001-TIMM                                
740012       MOVE INT-IDLANDX3-SEND TO W001-IDLANDX3-SEND                       
750012       MOVE INT-IDLANDX3-REC  TO W001-IDLANDX3-REC                        
760012       MOVE INT-KDVALISO      TO W001-KDVALISO                            
780012       MOVE INT-KDINTTYP      TO W001-KDINTTYP-OLD                        
780112       MOVE INT-DAFINDOC      TO W001-DAFINDOC                            
780464       MOVE INT-IDFINDOC      TO W001-IDFINDOC                            
780764       MOVE INT-IDSTATNR      TO W001-IDSTATNR                            
780864       MOVE INT-IDARTNR       TO W001-IDARTNR                             
780964       MOVE INT-BEART         TO W001-BEART                               
781064       MOVE INT-VKORDNTO      TO W001-VKORDNTO                            
781164       MOVE INT-KVLEVART      TO W001-KVLEVART                            
781264       MOVE INT-KDARTURS      TO W001-KDARTURS                            
781364       MOVE INT-SUNTO         TO W001-SUNTO                               
781464       MOVE INT-KDBEH         TO W001-KDBEH                               
781564       MOVE INT-IDDISTR       TO W001-IDDISTR                             
781664       MOVE INT-IDKUNDNR      TO W001-IDKUNDNR                            
781665       MOVE INT-IDVAT         TO W001-IDVAT                               
781666       MOVE INT-KDINTTYP      TO W001-KDINTTYP                            
781064       MOVE INT-VKORDNTO-3DEC TO W001-VKORDNTO-3DEC                       
781959       PERFORM S21-WRITE-W52214-001                                       
782064       MOVE INT-DAFINDOC      TO SPAR-DAFINDOC                            
782164       MOVE INT-IDFINDOC      TO SPAR-IDFINDOC                            
783010       PERFORM S01-READ-W52213                                            
790000     END-PERFORM                                                          
800000                                                                          
820000     PERFORM Z-FINIT                                                      
830000                                                                          
840000     MOVE ZERO TO RETURN-CODE                                             
850000     GOBACK                                                               
860000     .                                                                    
870000     EJECT                                                                
880000 A-INIT SECTION.                                                          
890100                                                                          
890208     OPEN INPUT  W52213                                                   
900220     OPEN OUTPUT W52214-001                                               
910000     SKIP2                                                                
920045*    ACCEPT TODAYS-DATE  FROM DATE                                        
930045*    MOVE TODAYS-DATE TO W001-DATE                                        
931000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
940000     .                                                                    
950000     EJECT                                                                
960000 Z-FINIT SECTION.                                                         
970108     CLOSE W52213                                                         
970320           W52214-001                                                     
980100     SKIP2                                                                
980200     MOVE 'S' TO POSTSUM-OPKOD                                            
981000     CALL POSTSUM USING POSTSUM-PARM                                      
990000     .                                                                    
000100     EJECT                                                                
000208 S01-READ-W52213  SECTION.                                                
000308     READ W52213 INTO W52213-AREA                                         
000400     AT END                                                               
000508        MOVE HIGH-VALUE TO W52213-AREA                                    
000608        SET END-OF-W52213 TO TRUE                                         
000700                                                                          
000800     NOT AT END                                                           
000920        MOVE 'W52213' TO POSTSUM-FDNAMN                                   
001020        MOVE 'W52214D1' TO POSTSUM-DDNAMN2                                
001108        MOVE 'INTRASTAT' TO POSTSUM-TRANSTYP                              
001200        CALL POSTSUM USING POSTSUM-PARM                                   
001300     END-READ                                                             
001400     .                                                                    
001500     EJECT                                                                
020220 S21-WRITE-W52214-001  SECTION.                                           
020300                                                                          
020461     IF W001-DAFINDOC NOT = SPAR-DAFINDOC OR                              
020561        W001-IDFINDOC NOT = SPAR-IDFINDOC                                 
020666       PERFORM S21A-WRITE-HEADERS                                         
020766     END-IF                                                               
022166     MOVE 1 TO W001-SKIP                                                  
023266     WRITE W52214-001-LINE FROM W001-LINE AFTER W001-SKIP                 
023656     SKIP2                                                                
023756     MOVE SPACE TO W001-LINE                                              
023956     .                                                                    
024056     EJECT                                                                
024132 S21A-WRITE-HEADERS SECTION.                                              
024232                                                                          
024332     ADD +1 TO W001-PAGECOUNTER                                           
024432     MOVE W001-PAGECOUNTER TO W001-PAGE                                   
024532     WRITE W52214-001-LINE FROM W001-HEADER1 AFTER PAGE                   
024632     WRITE W52214-001-LINE FROM W001-HEADER2 AFTER 2                      
024832     MOVE 3 TO W001-SKIP                                                  
024932     .                                                                    
025032     EJECT                                                                
