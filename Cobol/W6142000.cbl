000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6142000.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   JULI 2003.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER FIL W614PP MED URVAL FRÅN BILD 6308                        
000900*        SKAPAR UTFIL TILL MAIL.                                          
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDP3                                       
001200*                              W6D2                                       
001450                                                                          
001460     SKIP3                                                                
001470 ENVIRONMENT DIVISION.                                                    
001480     SKIP2                                                                
001490 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*          --- URVAL FRÅN 6308                                            
001900     SELECT W614PP                     ASSIGN TO W61420D1.                
002000     SKIP2                                                                
002100*          --- UTFIL FÖR LISTA                                            
002200     SELECT UTFIL                      ASSIGN TO W61420D2.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W614PP                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200 01  PARM             PIC X(80).                                          
003300     EJECT                                                                
003400 FD  UTFIL                                                                
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  UT-POST      -COPY W61420     -L.                                    
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004230 77  IDPGM                       PIC X(8)    VALUE 'W6142000'.            
004240 77  JA                          PIC X       VALUE 'J'.                   
004250 77  NEJ                         PIC X       VALUE 'N'.                   
004260 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
004270 77  MAX-IX                      PIC S9(3)   VALUE +7   COMP-3.           
004280 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004500                                                                          
004600 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
004700     88  END-OF-W614PP                       VALUE 'J'.                   
004918                                                                          
004919 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
004920                                                                          
004921 01  DYNAMISKA-SUBPROGRAM.                                                
004930     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004940     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004950     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004970                                                                          
004980 01  FELTEXT.                                                             
004990     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100     EJECT                                                                
005600*    --- PARAMETRAR TILL POSTSUM                                          
005700*01  -COPY W0005   -PRE  POSTSUM-                                         
005800     EJECT                                                                
005810 01  PARM-AREA-START             PIC X(24)   VALUE                        
005820                                 'PARM-AREA-START '.                      
005830 01  PARM-AREA                   PIC X(17).                               
005831 01  FILLER REDEFINES PARM-AREA.                                          
005832     03 PARM-IDARTNR             PIC 9(9).                                
005833     03 PARM-IDLEVNR             PIC X(5).                                
005834     03 PARM-IDPERSON            PIC 9(3).                                
005835     EJECT                                                                
006330 01  UT-AREA-START               PIC X(24)   VALUE                        
006340                                 'UT-AREA-START '.                        
006350 01  UT-AREA.                                                             
006370*    03  -COPY W61420  -PRE UT-                                           
006380     EJECT                                                                
006600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006700*                                                                         
006800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006900     SKIP3                                                                
007000 01  NYCKLAR-TILL-DLI.                                                    
007100     03  W-IDARTNR-X.                                                     
007200         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
007300     03  W-IDPERSON-X.                                                    
007400         05  W-IDPERSON          PIC S9(3)  VALUE ZERO COMP-3.            
007500     03  W-KDARBTYP-X.                                                    
007600         05  W-KDARBTYP          PIC X(8)   VALUE SPACE.                  
007700     03  W-IDKVAINF-MIN-X.                                                
007800         05  W-IDKVAINF-MIN      PIC 9(2)   VALUE ZERO.                   
007900     03  W-IDKVAINF-MAX-X.                                                
008000         05  W-IDKVAINF-MAX      PIC 9(2)   VALUE ZERO.                   
008010     03  W-KDKVAINF-MIN-X.                                                
008020         05  W-KDKVAINF-MIN      PIC X(1)   VALUE LOW-VALUE.              
008030     03  W-KDKVAINF-MAX-X.                                                
008040         05  W-KDKVAINF-MAX      PIC X(1)   VALUE HIGH-VALUE.             
008050     03  W-W6D211KY-X.                                                    
008060         05  W-DAREGDAT-9KOMPL   PIC 9(8)   VALUE 99999999.               
008070         05  W-TIKLOCK-9KOMPL    PIC S9(9)  VALUE ZERO COMP-3.            
008413     EJECT                                                                
008414*    --- STATUS-KOD FRÅN IMS                                              
008415 01  STATUS-WS                   PIC XX.                                  
008416     88  SEGMENT-FINNS                       VALUE '  '.                  
008418     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008419     SKIP2                                                                
008420 01  GODK-STATUSKODER.                                                    
008430     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008440     SKIP3                                                                
008450 01  SSA1                        PIC X(128).                              
008460 01  SSA2                        PIC X(64).                               
008470     EJECT                                                                
008480*    --- IMS FUNKTIONSKODER                                               
008490*01  -COPY W0003                                                          
008500     EJECT                                                                
008600*    ---  DLI INPUT-OUTPUT AREA                                           
010416 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6D201'.                      
010417 01  DLI-IO-W6D201.                                                       
010418*    03  -COPY W6D201                                                     
010419     EJECT                                                                
010420*    ---  DLI INPUT-OUTPUT AREA                                           
010421 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6D211'.                      
010422 01  DLI-IO-W6D211.                                                       
010423*    03  -COPY W6D211                                                     
010424     EJECT                                                                
010425 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDP311'.                      
010426 01  DLI-IO-WDP311.                                                       
010427*    03  -COPY WDP311                                                     
010428     EJECT                                                                
010429 LINKAGE SECTION.                                                         
010430                                                                          
010440*01  -COPY W0008  -PRE W6D2-                                              
010450     05  FILLER                  PIC X.                                   
010910     EJECT                                                                
010911*01  -COPY W0008  -PRE WDP3-                                              
010912     05  FILLER                  PIC X.                                   
010913     EJECT                                                                
010914 PROCEDURE DIVISION USING W6D2-PCB WDP3-PCB.                              
010915 MAIN SECTION.                                                            
010916     ENTRY 'DLITCBL' USING W6D2-PCB WDP3-PCB.                             
010917                                                                          
010918     PERFORM A-INIT                                                       
010919                                                                          
010920     PERFORM S11-LAES-W614PP                                              
010922     IF PARM-IDARTNR NOT = ZERO                                           
010923        PERFORM B-SKAPA-UTFIL                                             
010924     END-IF                                                               
010925                                                                          
010926     PERFORM Z-FINIT                                                      
010927                                                                          
010928     MOVE ZERO TO RETURN-CODE                                             
010929     GOBACK                                                               
010930     .                                                                    
010940     EJECT                                                                
011000 A-INIT SECTION.                                                          
011100                                                                          
011200     OPEN INPUT W614PP                                                    
011300     OPEN OUTPUT UTFIL                                                    
011400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011500     ACCEPT DAGENS-DATUM FROM DATE                                        
011600     MOVE DAGENS-DATUM TO UT-TIAAMMDD                                     
012000     PERFORM S01-NOLLSTALL                                                
012100     .                                                                    
012200     EJECT                                                                
012230 B-SKAPA-UTFIL SECTION.                                                   
012240                                                                          
012250     MOVE PARM-IDLEVNR TO UT-IDLEVNR                                      
012251     MOVE PARM-IDARTNR TO W-IDARTNR                                       
012252                          UT-IDARTNR                                      
012253                                                                          
012255     MOVE LOW-VALUE TO W-IDKVAINF-MIN-X                                   
012256                       W-KDKVAINF-MIN-X                                   
012257     MOVE HIGH-VALUE TO W-IDKVAINF-MAX-X                                  
012258                        W-KDKVAINF-MAX-X                                  
012259     MOVE 0          TO W-DAREGDAT-9KOMPL(1:1)                            
012260                                                                          
012261     PERFORM IMS-GET-W6D201                                               
012266     IF SEGMENT-FINNS                                                     
012268        PERFORM IMS-GET-W6D211                                            
012273        IF SEGMENT-FINNS                                                  
012275           IF INFO-IDKVAINF = 99                                          
012276              PERFORM IMS-GET-W6D211                                      
012277           END-IF                                                         
012278        END-IF                                                            
012279        IF SEGMENT-FINNS                                                  
012280           COMPUTE WS-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL          
012290           MOVE WS-DAREGDAT TO UT-DAREGDAT                                
012600                                                                          
014344           MOVE +1 TO IX                                                  
014345           PERFORM UNTIL IX > MAX-IX                                      
014346              MOVE INFO-TEKVAINF-EXT(IX) TO UT-TEKVAINF-EXT(IX)           
014348              INSPECT UT-TEKVAINF-EXT(IX) REPLACING                       
014349              ALL '#' BY SPACE                                            
014350              INSPECT UT-TEKVAINF-EXT(IX) REPLACING                       
014351              ALL 'Å' BY 'A'                                              
014352              INSPECT UT-TEKVAINF-EXT(IX) REPLACING                       
014353              ALL 'Ä' BY 'A'                                              
014354              INSPECT UT-TEKVAINF-EXT(IX) REPLACING                       
014355              ALL 'Ö' BY 'O'                                              
014356                                                                          
014357              ADD +1 TO IX                                                
014358           END-PERFORM                                                    
014362        END-IF                                                            
014363     END-IF                                                               
014364                                                                          
014365     MOVE 'QUAL    ' TO W-KDARBTYP                                        
014380     MOVE PARM-IDPERSON TO W-IDPERSON                                     
014381     PERFORM IMS-GU-WDP311                                                
014384     IF SEGMENT-FINNS                                                     
014385        MOVE PERS-IDNAMN TO UT-IDNAMN                                     
014386        MOVE PERS-IDMAIL TO UT-IDMAIL                                     
014387        MOVE PERS-IDTFN  TO UT-IDTFN                                      
014389     END-IF                                                               
014391     PERFORM S12-SKRIV-UTPOST                                             
014393     .                                                                    
014394     EJECT                                                                
014395 Z-FINIT SECTION.                                                         
014396                                                                          
014397     CLOSE W614PP                                                         
014398           UTFIL                                                          
014399                                                                          
014400     MOVE 'S' TO POSTSUM-OPKOD                                            
014401     CALL POSTSUM USING POSTSUM-PARM                                      
014402     .                                                                    
014403     EJECT                                                                
014404 S01-NOLLSTALL SECTION.                                                   
014410                                                                          
014420     MOVE SPACE TO UT-IDLEVNR                                             
014421                   UT-IDNAMN                                              
014422                   UT-IDMAIL                                              
014423                   UT-IDTFN                                               
014430     MOVE ZERO  TO UT-IDARTNR                                             
014480                   UT-DAREGDAT                                            
014491     MOVE +1 TO IX                                                        
014492     PERFORM UNTIL IX > MAX-IX                                            
014493        MOVE SPACE TO UT-TEKVAINF-EXT(IX)                                 
014503        ADD +1 TO IX                                                      
014505     END-PERFORM                                                          
014533     .                                                                    
014534     EJECT                                                                
014535 S11-LAES-W614PP SECTION.                                                 
014536                                                                          
014537     READ W614PP INTO PARM-AREA                                           
014549     .                                                                    
014550     EJECT                                                                
014551 S12-SKRIV-UTPOST SECTION.                                                
014552                                                                          
014553     WRITE UT-POST FROM UT-AREA                                           
014554                                                                          
014555     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014556     MOVE 'UTFIL  '  TO POSTSUM-FDNAMN                                    
014557     MOVE 'W61420D2' TO POSTSUM-DDNAMN2                                   
014558     CALL POSTSUM USING POSTSUM-PARM                                      
014559     .                                                                    
014560     EJECT                                                                
014625* --- IMS SEKTIONER ---                                                   
014626     SKIP3                                                                
015934 IMS-GET-W6D201 SECTION.                                                  
015935     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
015936             DELIMITED BY SIZE INTO SSA1                                  
015938     MOVE '  GE' TO GODK-STATUSKODER                                      
015939     CALL CBLTDLI USING GU W6D2-PCB DLI-IO-W6D201 SSA1                    
015940     MOVE W6D2-STATUS-CODE TO STATUS-WS                                   
015941     PERFORM IMS-STATUSKONTROLL                                           
015942     .                                                                    
015943     SKIP3                                                                
015945 IMS-GET-W6D211 SECTION.                                                  
015949     MOVE 'W6D211  ' TO SSA1                                              
015950     MOVE '  GE' TO GODK-STATUSKODER                                      
015951     CALL CBLTDLI USING GNP W6D2-PCB DLI-IO-W6D211 SSA1                   
015952     MOVE W6D2-STATUS-CODE TO STATUS-WS                                   
015953     PERFORM IMS-STATUSKONTROLL                                           
015954     .                                                                    
015955     EJECT                                                                
015982 IMS-GU-WDP311 SECTION.                                                   
015983     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
015984          DELIMITED BY SIZE INTO SSA1                                     
015985     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
015986          DELIMITED BY SIZE INTO SSA2                                     
015987     MOVE '  GE' TO GODK-STATUSKODER                                      
015988     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
015989     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
015990     PERFORM IMS-STATUSKONTROLL                                           
015991     .                                                                    
015992     EJECT                                                                
015993 IMS-STATUSKONTROLL SECTION.                                              
015994                                                                          
015995     SET STATUS-IX TO 1                                                   
015996     SEARCH GODK-STATUS                                                   
015997       AT END                                                             
015998         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015999           DELIMITED BY SIZE INTO FELTEXT                                 
016000         DISPLAY FELTEXT                                                  
016001         CALL FELLOG                                                      
016002       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016003         CONTINUE                                                         
016010     END-SEARCH                                                           
016100     .                                                                    
