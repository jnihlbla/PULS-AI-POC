000100*** EDIT ALLOWED                                                          
000200 01  T335R309.                                                            
000300*        REQUISITION DEVIATION AND CANCEL TO PULS                         
000400*                        ANV I PGM   ?                                    
050099*                                                                         
060099     03  IDRT                   PIC   X(3)  VALUE '309'.                  
070099*                                    RECORD TYPE                          
080099*                                                                         
090099     03  COMMON-AREA.                                                     
100099         05 CDTYPE-REQ          PIC   X(2)  VALUE 'PR'.                   
110099*                                    REQ TYPE                             
120099         05 IDPITEM-GRP.                                                  
130099            07 IDPITEM          PIC   X(20).                              
140099*                                    PURCHASED ITEM NO  JUST R            
150099            07 CD-IDPITEM       PIC   X(1)  VALUE 'V'.                    
160099*                                    PURCHASED ITEM QUALIF                
170099         05 NMPITEM             PIC   X(25) VALUE SPACE.                  
180099*                                    PARTNAME GB                          
190099         05 IDPORG              PIC   X(4)  VALUE 'VCCS'.                 
200099*                                    PURCHASE ORGANIZATION                
210099         05 IDHANDLR            PIC   9(4).                               
220099*                                    IDANSK ?                             
230099         05 TXNOTES-REQ1        PIC   X(70) VALUE SPACE.                  
240099*                                    NOTES LINE 1                         
250099         05 TXNOTES-REQ2        PIC   X(70) VALUE SPACE.                  
260099*                                    NOTES LINE 2                         
270099         05 NMHANDLR-ISSUER     PIC   X(25) VALUE SPACE.                  
280099*                                    HANDLER NAME                         
290099         05 IDPHONE-ISSUER      PIC   X(20) VALUE SPACE.                  
300099*                                    TELEPHONE NO                         
310099         05 IDSECTN-ISSUER      PIC   X(6)  VALUE SPACE.                  
320099*                                    SECTION                              
330099         05 IDUSERID-ISSUER     PIC   X(8)  VALUE SPACE.                  
340099*                              CDS ID FOR ISSUER   NB CDS ID              
350099         05 FILLER              PIC   X(30) VALUE SPACE.                  
360099*                                                                         
370099     03  NP-AREA.                                                         
380099         05 NP-IDUSER           PIC   X(5)  VALUE SPACE.                  
390099*                              MATERIAL USER IDENTITY   GSDB ID           
400099         05 NP-CDUOM            PIC   X(3)  VALUE SPACE.                  
410099*                                    UNIT OF MEASUREMENT    PCE           
420099         05 NP-TIPROD-DATE      PIC   9(6)  VALUE ZERO.                   
430099*                                    PROD DATE YYMMDD                     
440099         05 NP-QTPITEM-YEAR     PIC   9(9)  VALUE ZERO.                   
450099*                                    PART QUANTITY PER YEAR               
460099         05 FILLER              PIC   X(30) VALUE SPACE.                  
470099*                                                                         
480099     03  CC-AREA.                                                         
480199         05 CC-IDUSER           PIC   X(5)  VALUE SPACE.                  
480299*                              MATERIAL USER IDENTITY   GSDB ID           
480399         05 CC-TIEXIT-WEEK      PIC   X(4)  VALUE SPACE.                  
480499*                                    YYWW CANCEL WEEK                     
481099         05 FILLER              PIC   X(30) VALUE SPACE.                  
490099*                                                                         
500099     03  RETURN-AREA.                                                     
510099         05 RETURN-CODE         PIC   X(2)  VALUE ZERO.                   
511099*                     '  ' = OK                                           
511100*                     '00' = OK                                           
512099*                     '02' = OK, REQUISITION UPDATED (PR)                 
512199*                     '04' = DIFFERENT CURRENT BUYER IN SI+ (PR)          
512200*                     '05' = ALREADY ORDERED, REQ NOT UPDATED(PR)         
513099*                     '08' = VALIDATION ERROR                             
513199*                     '10' = ORDER NOT FOUND (CC)                         
514099*                     '12' = ALREADY ORDERED, REQ NOT UPDATED(PR)         
515099*                     '20' = CANCELED BY BUYER                            
520099         05 RETURN-IDPORG       PIC   X(4)  VALUE SPACE.                  
530099         05 RETURN-IDHANDLR     PIC   9(4)  VALUE ZERO.                   
540099         05 RETURN-REASON-1     PIC   X(70) VALUE SPACE.                  
550099         05 RETURN-REASON-2     PIC   X(70) VALUE SPACE.                  
560099         05 RETURN-ARRIVED      PIC   9(6)  VALUE ZERO.                   
570099         05 RETURN-IDUSERID-DELETER PIC X(8) VALUE SPACE.                 
580099         05 RETURN-DELETE-DATE  PIC   9(6)  VALUE ZERO.                   
590099         05 FILLER              PIC   X(30) VALUE SPACE.                  
600099*                                                                         
600100*                                                                         
