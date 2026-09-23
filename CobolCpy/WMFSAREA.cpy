001000*** EDIT ALLOWED                                                          
010000 01      WMFSAREA.                                                        
050011*                                 GENERAL AREAS FOR MFS PROCESSING        
080000   03    MFS-IDTRANS         PIC X(4)        VALUE SPACE.                 
090000*                                 BILDNR FÖR MID FRÅN VILKEN              
100000*                                 MEDDELANDET KOMMER.                     
110000*                                 SCREEN NBR OF SENDING MID.              
120000   03    MFS-KDMFSFOR        PIC X(1)        VALUE SPACE.                 
130000*                                 TYP AV MFS-FORMAT (5:e SIFFRAN)         
140000*                                 5:th DIGIT IN SCREEN NBR.               
160000         88  SWEDISH-TEXT                    VALUE '1'.                   
180000         88  ENGLISH-TEXT                    VALUE '2'.                   
190000*                                                                         
210001   03    MFS-KDTRTYP         PIC X(1)        VALUE SPACE.                 
240000*                                 TYPE OF TRANSACTION                     
250000*                                 (POS 7 IN TRANSACTION CODE).            
280000         88  MFS-QUERY                       VALUE ' '.                   
290000*                                 FRÅGETRANSAKTION.                       
300000*                                 QUERY TRANSACTION.                      
320000         88  MFS-UPDATE                      VALUE 'U'.                   
330000*                                 PF11 ÄR TRYCKT FÖR UPPDATERING.         
340000*                                 PF11 PRESSED FOR UPDATE.                
350008         88  MFS-UPD-V                       VALUE 'V'.                   
360000*                                 PF23 ÄR TRYCKT FÖR UPPDATERING.         
370000*                                 PF23 PRESSED FOR UPDATE.                
380008         88  MFS-UPD-X                       VALUE 'X'.                   
390000*                                 UPPDATERINGSTRANS FRÅN                  
400000*                                 ETT ANNAT PROGRAM.                      
410000*                                 UPDATING TRANSACTION FROM               
420000*                                 ANOTHER PROGRAM.                        
421008         88  MFS-UPD-Y                       VALUE 'Y'.                   
422005*                                 UPPDATERINGSTRANS FRÅN                  
422105*                                 ETT ANNAT PROGRAM.                      
422205*                                 Y-TRANS HAR ANNAN PRIORITET.            
422305*                                 UPDATING TRANSACTION FROM               
422405*                                 ANOTHER PROGRAM.                        
430000*                                                                         
440007   03    MFS-IDPFK           PIC X(1)        VALUE SPACE.                 
450000*                                 ANGER VILKEN PFK SOM ÄR TRYCKT          
460000*                                 OM DET FINNS 2 TRANSKODER.              
470000*                                 SPECIFIES PF KEY IF THERE ARE           
480000*                                 DOUBLE TRANSACTION CODES.               
490016         88  MFS-RETURN                      VALUE '3'.                   
491016         88  MFS-PRINT                       VALUE '4'.                   
500000         88  MFS-PREVIOUS                    VALUE '6'.                   
500100         88  MFS-FIRST                       VALUE '7'.                   
510000         88  MFS-NEXT                        VALUE '8'.                   
520000         88  MFS-ENTER                       VALUE ' '.                   
530000         88  MFS-SPLIT                       VALUE '9'.                   
540000*                                                                         
550007   03    MFS-IDMOD.                                                       
560000*                                 HIT FLYTTAS NAMNET PÅ DEN MOD           
570000*                                 SOM SKALL SKRIVAS.                      
580000*                                 MOVE NAME OF MOD TO BE WRITTEN          
590000*                                 TO THIS FIELD.                          
591006     05  FILLER              PIC X(6)    VALUE SPACE.                     
600006     05  MFS-KDHUVOMR        PIC X       VALUE SPACE.                     
611006*                                 SKALL VARA 0 ELLER N.                   
620006*                                 POS NO. 7 IN MOD NAME.                  
630006     05  FILLER              PIC X(1)    VALUE SPACE.                     
640000*                                                                         
650000*                                                                         
660000*           SPECIALKOMANDON - SPECIAL COMMANDS                            
670000*                                                                         
680000   03    MFS-RENSA-ALLA-OEPPNA-FAELT.                                     
690000    04   MFS-ERASE-ALL-OPEN-FIELDS.                                       
700000*                                 FLYTTAS TILL SCA FÖR ATT RENSA          
710000*                                 ALLA ÖPPNA FÄLT MED ETT KOMMANDO        
720000*                                 MOVE TO SCA TO ERASE ALL OPEN           
730000*                                 FIELDS WITH ONE COMMAND.                
740000      05 FILLER              PIC S9(4)       VALUE +160  COMP.            
750000*                                            HEX '00A0'                   
760000   03    MFS-RENSA-SKAERMEN.                                              
770000    04   MFS-ERASE-SCREEN.                                                
780000*                                 FLYTTAS TILL SCA FÖR ATT RENSA          
790000*                                 HELA SKÄRMEN FÖRE SKRIVNING             
800000*                                 (FUNGERAR SOM CLEAR)                    
810000*                                 MOVE TO SCA TO ERASE SCREEN             
820000*                                 (WORKS LIKE CLEAR FROM KEYBOARD)        
830000     05  FILLER              PIC S9(4)       VALUE +192  COMP.            
840000*                                            HEX '00C0'                   
850000*                                                                         
860000*                                                                         
870000*           DESSA FÄLT FLYTTAS TILL DATAAREAN.                            
880000*           MOVE THESE FIELDS TO DATA FIELD AREA.                         
890000*                                                                         
900000   03    MFS-RENSA-FAELT.                                                 
910000    04   MFS-ERASE-FIELD.                                                 
920000*                                 ANVÄNDS FÖR ATT RENSA FÄLT              
930000*                                 GER HEX '00' PÅ SKÄRMEN                 
940000*                                 USED TO ERASE A FIELD                   
950000*                                 GIVES HEX '00' ON SCREEN.               
960000     05  FILLER              PIC 9(3)        VALUE 53    COMP-3.          
970000*                                            HEX '053F'                   
980000   03    MFS-BLANKA-UT-FAELT.                                             
990000    04   MFS-BLANK-FIELD.                                                 
000000*                                 ANVÄNDS FÖR ATT BLANKA UT FÄLT          
010000*                                 GER 1 BLANK + RESTEN HEX '00'.          
020000*                                 USED TO BLANK OUT A FIELD               
030000*                                 GIVES 1 BLANK + HEX '00'S               
040000     05  FILLER              PIC 9(3)        VALUE 403   COMP-3.          
050000*                                            HEX '403F'                   
060000   03    MFS-ROER-EJ-FAELT.                                               
070000    04   MFS-DO-NOT-TOUCH-FIELD.                                          
080000*                                 LÄMNAR FÄLT ORÖRDA                      
090000*                                 INGEN ÖVERFÖRING TILL SKÄRMEN           
100000*                                 USED TO LEAVE FIELD UNTOUCHED           
110000*                                 NO TRANSMISSION TO SCREEN               
120000     05  FILLER              PIC 9(1)        VALUE 3     COMP-3.          
130000*                                            HEX '3F'                     
140000*                                                                         
150000*                                                                         
160000*           DESSA FÄLT FLYTTAS TILL ATTRIBUT FÖRE DATAFÄLT                
170000*           MOVE THESE FIELDS TO ATTRIBUTE IN FRONT OF DATA               
180000*                                                                         
190000   03    MFS-FORMATETS-ATTR.                                              
200000    04   MFS-FORMAT-DEFAULT-ATTR.                                         
210000*                                 FORMATETS GRUND-ATTRIBUT                
220000*                                 LÄGGS UT PÅ SKÄRMEN.                    
230000*                                 ATTRIBUTE FROM FORMAT IS SENT           
240000*                                 TO SCREEN.                              
250000     05  FILLER              PIC S9(4)      VALUE ZERO  COMP.             
260000*                                           HEX '0000'                    
270000   03    MFS-ADD-LYS-UPP-FAELT.                                           
280000    04   MFS-ADD-HILIGHT-FIELD.                                           
290000*                                 ANVÄNDS FÖR ATT LYSA UPP                
300000*                                 UTMATNINGS-FÄLT                         
310000*                                 USED TO LIGHT UP AN OUTPUT FLD          
320000*                                 (BASIC + HIGH + FORMAT ATTR)            
330000     05  FILLER              PIC S9(4)       VALUE +136  COMP.            
340000*                                            HEX '0088'                   
350000   03    MFS-ADD-LAES-IN-FAELT.                                           
360000    04   MFS-ADD-READ-FIELD.                                              
370000*                                 ANVÄNDS NÄR MAN VILL LÄSA IN            
380000*                                 ETT FÄLT IGEN                           
390000*                                 USED TO READ A FIELD AGAIN              
400000*                                 (BASIC + MOD + FORMAT ATTR)             
410000     05  FILLER              PIC S9(4)       VALUE +129  COMP.            
420000*                                            HEX '0081'                   
430000   03    MFS-ADD-LAES-IN-FAELT-HI.                                        
440000    04   MFS-ADD-READ-HILIGHT-FIELD.                                      
450000*                                 ANVÄNDS FÖR ATT LYSA UPP ETT            
460000*                                 FÄLT OCH SEDAN LÄSA IN DET IGEN         
470000*                                 USED TO HILIGHT A FIELD AND             
480000*                                 THEN READ IT AGAIN                      
490000*                                 (BASIC + MOD + FORMAT ATTR              
500000*                                  + HILIGHT)                             
510000     05  FILLER              PIC S9(4)       VALUE +137  COMP.            
520000*                                            HEX '0089'                   
530000   03    MFS-ADD-SAETT-CURSOR.                                            
540000    04   MFS-ADD-SET-CURSOR.                                              
550000*                                 ANVÄNDS FÖR ATT FLYTTA CURSOR           
560000*                                 TILL ETT FÄLT                           
570000*                                 USED TO MOVE CURSOR TO A FIELD          
580000*                                 (CURSOR + BASIC + FORMAT ATTR)          
590000     05  FILLER              PIC X(2)        VALUE 'äØ'.                  
600000*                                            HEX 'C080'                   
620014   03    MFS-OPEN-ALPHA-NOMOD.                                            
630012*                                 FÖR ATT ÖPPNA ETT ALFA FÄLT             
660012*                                 USED TO OPEN ALPHANUM FIELD             
670000*                                 BASIC + REPL + UNPROT                   
680000     05  FILLER              PIC X(2)       VALUE ' ä'.                   
690000*                                           HEX '00C0'                    
710014    04   MFS-OPEN-NUM-NOMOD.                                              
720012*                                 FÖR ATT ÖPPNA ETT NUMERISKT FÄLT        
740012*                                 USED TO OPEN NUMERIC FIELD              
750000*                                 (BASIC + REPL + NUM + UNPROT)           
760000     05  FILLER              PIC X(2)       VALUE ' å'.                   
770000*                                           HEX '00D0'                    
780000   03    MFS-OEPPNA-ALFA-FAELT.                                           
790000    04   MFS-OPEN-ALPHA-FIELD.                                            
800012*                                 FÖR ATT ÖPPNA OCH LÄSA IN               
810012*                                 ETT ALFANUM INMATNINGS-FÄLT.            
820012*                                 USED TO OPEN AND READ IN                
821012*                                 A ALPHA INPUT FIELD                     
830000*                                 (CURSOR + BASIC + REPL + MOD)           
840000     05  FILLER              PIC X(2)       VALUE 'äA'.                   
850000*                                           HEX 'C0C1'                    
860000   03    MFS-OEPPNA-ALFA-FAELT-HI.                                        
870000    04   MFS-OPEN-ALPHA-FIELD-HI.                                         
880000*                                 FÖR ATT ÖPPNA OCH LYSA UPP              
890000*                                 ETT ALFA INMATNINGS-FÄLT                
900000*                                 USED TO OPEN AND HILIGHT AN             
910000*                                 ALHPANUMERIC INPUT FIELD                
920010*                                 (CURSOR + BASIC + REPL + HIGH           
921010*                                 + MOD)                                  
930010     05  FILLER              PIC X(2)        VALUE 'äI'.                  
940015*                                            HEX 'C0C9'                   
950000   03    MFS-OEPPNA-NUM-FAELT.                                            
960000    04   MFS-OPEN-NUM-FIELD.                                              
970012*                                 FÖR ATT ÖPPNA OCH LÄSA IN               
980012*                                 ETT NUMERISKT INMATNINGS-FÄLT.          
990012*                                 USED TO OPEN AND READ IN                
000012*                                 A NUMERIC INPUT FIELD.                  
010000*                                 (CURSOR + BASIC + REPL + NUM            
020000*                                 + MOD)                                  
030000     05  FILLER              PIC X(2)        VALUE 'äJ'.                  
040000*                                            HEX 'C0D1'                   
050000   03    MFS-OEPPNA-NUM-FAELT-HI.                                         
060000    04   MFS-OPEN-NUM-FIELD-HI.                                           
070000*                                 FÖR ATT ÖPPNA OCH LYSA UPP              
080000*                                 ETT NUMERISKT INMATNINGS-FÄLT           
090000*                                 USED TO OPEN AND LIGHT UP A             
100000*                                 NUMERIC INPUT FIELD.                    
110000*                                 (CURSOR + BASIC + REPL + NUM            
120000*                                 + HIGH                                  
130000     05  FILLER              PIC X(2)        VALUE 'äQ'.                  
140000*                                            HEX 'C0D8'                   
150000   03    MFS-STAENG-FAELT.                                                
160000    04   MFS-CLOSE-FIELD.                                                 
170000*                                 FÖR ATT STÄNGA BÅDE NUMERISKT           
180000*                                 OCH ALFA INMATNINGS-FÄLT.               
190000*                                 USED TO CLOSE BOTH NUMERIC              
200000*                                 AND ALPHANUM INPUT FIELDS               
210013*                                 (BASIC + REPL + PROT + NUM + MOD        
230000     05  FILLER              PIC S9(4)       VALUE +241  COMP.            
240000*                                            HEX '00F1'                   
241003   03    MFS-STAENG-FAELT-HI.                                             
242003    04   MFS-CLOSE-FIELD-HI.                                              
243003*                                 FÖR ATT STÄNGA, LÄSA-IN OCH             
244003*                                 LYSA UPP ETT INMATNINGS-FÄLT.           
245003*                                 USED TO CLOSE, READ-IN AND              
246003*                                 LIGHT UP A INPUT FIELD.                 
247003*                                 (BASIC + REPL + PROT + NUM              
248004*                                 + HI + MOD)                             
249004     05  FILLER              PIC S9(4)       VALUE +249  COMP.            
249103*                                            HEX '00F9'                   
250000   03    MFS-STAENG-FAELT-NOMOD.                                          
260000    04   MFS-CLOSE-FIELD-NOMOD.                                           
270000*                                 FÖR ATT STÄNGA BÅDE NUMERISKT           
280000*                                 OCH ALFA INMATNINGS-FÄLT                
290000*                                 OCH EJ LÄSA IN DATAT IGEN.              
300000*                                 USED TO CLOSE BOTH NUMERIC              
310000*                                 AND ALPHANUM INPUT FIELDS NOMOD         
320000*                                 (BASIC + REPL + PROT + NUM)             
330000     05  FILLER              PIC S9(4)       VALUE +240  COMP.            
340000*                                            HEX '00F0'                   
350000   03    MFS-STAENG-FAELT-OSYNLIGT.                                       
360000    04   MFS-CLOSE-NONDISP-FIELD.                                         
370000*                                 FÖR ATT STÄNGA ETT OSYNLIGT             
380000*                                 INMATNINGS-FÄLT (ALFA ELLER NUM)        
390000*                                 USED TO CLOSE A NONDISPLAYABLE          
400000*                                 INPUT FIELD (ALPHA OR NUMERIC)          
410000*                                 (BASIC + REPL + PROT + NUM              
420000*                                 + NONDISP + MOD)                        
430000     05  FILLER              PIC S9(4)       VALUE +245  COMP.            
440000*                                            HEX '00F5'                   
450000   03    MFS-ALFA-FAELT-RAETT.                                            
460000    04   MFS-ALPHA-FIELD-OK.                                              
470000*                                 FÖR ATT MARKERA ETT                     
480000*                                 RÄTT INMATAT ALFA FÄLT.                 
490000*                                 USED TO MARK AN ALPHA FIELD             
500000*                                 AS CORRECTLY ENTERED.                   
510000*                                 (BASIC + REPL + MOD)                    
520000     05  FILLER              PIC S9(4)       VALUE +193  COMP.            
530000*                                            HEX '00C1'                   
540000   03    MFS-NUM-FAELT-RAETT.                                             
550000    04   MFS-NUM-FIELD-OK.                                                
560000*                                 FÖR ATT MARKERA ETT                     
570000*                                 RÄTT INMATAT NUMERISKT FÄLT.            
580000*                                 USED TO MARK A NUMERIC FIELD            
590000*                                 AS CORRECTLY ENTERED.                   
600000*                                 (BASIC + REPL + NUM + MOD)              
610000     05  FILLER              PIC S9(4)       VALUE +209  COMP.            
620000*                                            HEX '00D1'                   
630000   03    MFS-ALFA-FAELT-FEL.                                              
640000    04   MFS-ALPHA-FIELD-WRONG.                                           
650000*                                 FÖR ATT MARKERA ETT                     
660000*                                 FEL INMATAT ALFA FÄLT                   
670000*                                 USED TO MARK AN ALPHA INPUT             
680000*                                 FIELD AS INVALID.                       
690000*                                 (CURSOR + BASIC + REPL                  
700000*                                 + HIGH + MOD)                           
710000     05  FILLER              PIC X(2)        VALUE 'äI'.                  
720000*                                            HEX 'C0C9'                   
730000   03    MFS-NUM-FAELT-FEL.                                               
740000    04   MFS-NUM-FIELD-WRONG.                                             
750000*                                 FÖR ATT MARKERA ETT                     
760000*                                 FEL INMATAT NUMERISKT FÄLT              
770000*                                 USED TO MARK A NUMERIC INPUT            
780000*                                 FIELD AS INVALID.                       
790000*                                 (CURSOR + BASIC + REPL + NUM            
800000*                                 + HIGH + MOD)                           
810000     05  FILLER              PIC X(2)        VALUE 'äR'.                  
820000*                                            HEX 'C0D9'                   
830015   03    MFS-ALPHA-UPDATE-OK.                                             
850015*                                 FÖR ATT LYSA UPP ETT ALFA FÄLT          
860015*                                 SOM HAR UPPDATERATS.                    
870015*                                 TO HILIGHT AN ALPHA FIELD               
880015*                                 WHO HAS BEEN UPDATED.                   
890015*                                 (BASIC + REPL + HI)                     
910015     05  FILLER              PIC X(2)        VALUE ' H'.                  
920015*                                            HEX '00C8'                   
930015   03    MFS-NUM-UPDATE-OK.                                               
940015*                                 FÖR ATT LYSA UPP ETT num FÄLT           
941015*                                 SOM HAR UPPDATERATS.                    
942015*                                 TO HILIGHT AN ALPHA FIELD               
943015*                                 WHO HAS BEEN UPDATED.                   
944015*                                 (BASIC + REPL + NUM + HI)               
010015     05  FILLER              PIC X(2)        VALUE ' Q'.                  
020015*                                            HEX '00D8'                   
030011*** END COPY WMFSAREA    LENGTH=50    OLD LENGTH=                         
