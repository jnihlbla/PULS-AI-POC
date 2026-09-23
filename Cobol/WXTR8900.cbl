001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     WXTR8900.                                                
001200 AUTHOR.         BODIL LINDAHL.                                           
001300 DATE-WRITTEN.   00/09/25.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PROGRAMMET RÄKNAR UT REOSAEK OSÄKERHETSFAKTOR FÖR                
001800*        SDC/NDC/LDC -ARTIKLAR.                                           
001900*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- ARTIKELFIL                                                 
003303     SELECT W01184                     ASSIGN TO WXTR89D1.                
003304     SKIP2                                                                
003305*          --- REOSAEK                                                    
003310     SELECT WXTR89                     ASSIGN TO WXTR89D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W01184                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003906*01  -COPY W01184                    -L.                                  
003907                                                                          
003908 FD  WXTR89                                                               
003909     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003920*01  POST   -COPY WXTR89  -PRE UT-   -L.                                  
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004201                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'WXTR8900'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004701                                                                          
004702 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
004710     88  END-OF-W01184                       VALUE 'J'.                   
004800                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006101     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006110     03  W271SEAS                PIC X(8)    VALUE 'W271SEAS'.            
006200                                                                          
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
008002*    --- PARAMETRAR TILL POSTSUM                                          
008003*                                                                         
008010*01  -COPY W0005   -PRE  POSTSUM-                                         
008103     EJECT                                                                
008104*    --- PARAMETRAR TILL W271SEAS                                         
008110*01  -COPY W271SEAS                                                       
008201     EJECT                                                                
008202 01  IN-AREA-START               PIC X(24)   VALUE                        
008203                                 'IN-AREA-START  '.                       
008206*01  IN-AREA -COPY W01184                                                 
008207     EJECT                                                                
008208 01  UT-AREA-START               PIC X(24)   VALUE                        
008209                                 'UT-AREA-START  '.                       
008220*01  AREA -COPY WXTR89   -PRE UT-                                         
008300     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011500*01  -COPY W0008  -PRE WDK7-                                              
011510     05  FILLER                  PIC X.                                   
011520     EJECT                                                                
011530*01  -COPY W0008  -PRE WDL7-                                              
011540     05  FILLER                  PIC X.                                   
011550     EJECT                                                                
011551*01  -COPY W0008  -PRE WDL4-                                              
011552     05  FILLER                  PIC X.                                   
011553     EJECT                                                                
011560*01  -COPY W0008  -PRE WDK6-                                              
011570     05  FILLER                  PIC X.                                   
011580     EJECT                                                                
011590*01  -COPY W0008      -PRE WDB6-                                          
011591     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011701 PROCEDURE DIVISION  USING WDK7-PCB WDL7-PCB WDL4-PCB WDK6-PCB            
011703                                                      WDB6-PCB.           
011704 MAIN SECTION.                                                            
011710     ENTRY 'DLITCBL' USING WDK7-PCB WDL7-PCB WDL4-PCB WDK6-PCB            
011800                                                      WDB6-PCB.           
012100     PERFORM A-INIT                                                       
012200                                                                          
012310     PERFORM S01-LAES-W01184                                              
012400     PERFORM UNTIL END-OF-W01184                                          
012500        PERFORM B-BEHANDLA-SKRIV                                          
013110        PERFORM S01-LAES-W01184                                           
013200     END-PERFORM                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014201                                                                          
014210     OPEN INPUT  W01184                                                   
014310     OPEN OUTPUT WXTR89                                                   
014320                                                                          
014330     MOVE ZERO  TO UT-IDARTNR                                             
014331                   UT-REOSAEK                                             
014340     MOVE SPACE TO UT-IDDC                                                
015100     .                                                                    
015200     EJECT                                                                
015210 B-BEHANDLA-SKRIV SECTION.                                                
015211                                                                          
015213     MOVE SLAG-IDDC    TO SEAS-IDDC                                       
015214     MOVE SLAG-IDARTNR TO SEAS-IDARTNR                                    
015215     MOVE NEJ          TO SEAS-FLKVARTAL                                  
015216                                                                          
015217     CALL W271SEAS USING SEAS-W271SEAS WDK7-PCB WDL7-PCB WDL4-PCB         
015218                         WDK6-PCB WDB6-PCB                                
015219                                                                          
015220     IF SEAS-KDSVAR = SPACE                                               
015221        MOVE SEAS-OSAKERHET TO UT-REOSAEK                                 
015226     ELSE                                                                 
015227        MOVE ZERO           TO UT-REOSAEK                                 
015228     END-IF                                                               
015229     PERFORM BA-SKAPA-SKRIV-UTPOST                                        
015230     .                                                                    
015231     SKIP3                                                                
015240 BA-SKAPA-SKRIV-UTPOST SECTION.                                           
015241                                                                          
015242     MOVE SLAG-IDDC    TO UT-IDDC                                         
015243     MOVE SLAG-IDARTNR TO UT-IDARTNR                                      
015245     PERFORM S11-SKRIV-WXTR89                                             
015246                                                                          
015247     MOVE ZERO  TO UT-IDARTNR                                             
015248                   UT-REOSAEK                                             
015249     MOVE SPACE TO UT-IDDC                                                
015250     .                                                                    
015260     EJECT                                                                
015300 Z-FINIT SECTION.                                                         
015400                                                                          
015401     CLOSE W01184                                                         
015410           WXTR89                                                         
015502     MOVE 'S' TO POSTSUM-OPKOD                                            
015510     CALL POSTSUM USING POSTSUM-PARM                                      
015600     .                                                                    
015701     EJECT                                                                
015702 S01-LAES-W01184  SECTION.                                                
015703                                                                          
015704     READ W01184 INTO IN-AREA                                             
015705     AT END                                                               
015707        SET END-OF-W01184 TO TRUE                                         
015708                                                                          
015709     NOT AT END                                                           
015710        MOVE 'W01184'   TO POSTSUM-FDNAMN                                 
015711        MOVE 'WXTR89D1' TO POSTSUM-DDNAMN2                                
015713        MOVE SPACE      TO POSTSUM-TRANSTYP                               
015714        CALL POSTSUM USING POSTSUM-PARM                                   
015715     END-READ                                                             
015720     .                                                                    
015801     SKIP3                                                                
015802 S11-SKRIV-WXTR89 SECTION.                                                
015803                                                                          
015804     WRITE UT-POST FROM UT-AREA                                           
015805                                                                          
015806     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
015807     MOVE 'WXTR89'   TO POSTSUM-FDNAMN                                    
015808     MOVE 'WXTR89D2' TO POSTSUM-DDNAMN2                                   
015809     CALL POSTSUM USING POSTSUM-PARM                                      
015810     .                                                                    
016000     EJECT                                                                
