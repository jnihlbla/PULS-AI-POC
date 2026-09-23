//WBATCHZ1 JOB (540W0090100WBATCHZ1,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
 IF-CALENDAR AFTERWEEK                                                          
   START WBATCHQ1                                                               
 END-IF                                                                         
 IF-CALENDAR BATCH01                                                            
   START WBATCHQ1                                                               
 END-IF                                                                         
//*  CANCEL WXXXXXX                                                             
//*  --                                                                         
//*  --  OM EN RUTIN I WBATCH2 SKA STÄLLAS IN I KOMMANDE KÖRNINGAR              
//*  --  UNDER KVÄLLEN/NATTEN PGA OUPPKLARAD ABEND I NÅGON AV DE                
//*  --  FÖRSTA KÖRNINGARNA, GÖR SÅ HÄR:                                        
//*  --  (1) STARTA EN JCL-TEMPÄNDRING AV DETTA JOBB (SOP "J").                 
//*  --  (2) GÖR I ORDNING ETT FUNGERANDE CANCEL-KOMMANDO AV FÖRSTA             
//*  --      RADEN I DENNA KOMMENTAR GENOM ATT TA BORT JCL-                     
//*  --      KOMMENTAREN (//*) OCH ANGE ÖNSKAD RUTIN.                           
//*  --  (3) SPARA TEMP-ÄNDRINGEN OCH ANGE ATT DEN SKA GÄLLA SÅ                 
//*  --      MÅNGA GÅNGER SOM RUTINEN SKA STÄLLAS IN (1 ELLER 2 GGR)            
//*  --                                                                         
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WBATCHZ1                                         
