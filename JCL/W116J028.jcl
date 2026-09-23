//W116J028 JOB (640W1160100W116J028,W100),'RTN W116S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//********************************************************************          
//*         VCOM-ÖVERFÖRING AV ARTIKELINFO                           *          
//********************************************************************          
//*                                                                             
//* ---------- STEG 1. SKAPA EN 'DEL-FIL' ATT SKICKA -----------------          
//*                                                                             
//W116     EXEC W116P028                                                        
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA DISTRIBUTION PRINT-------          
//*                                                                             
//EMPTY EXEC WEMPTST,DSIN=W116.W116S2.W11628(+1)                                
//    IF (EMPTY.T.RC = 0) THEN                                                  
// EXEC WZ14DAP4,DSIN=W116.W116S2.W11628(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W11628 -------          
//*                                                                             
//SOPBEST EXEC WSOP,COND=(8,NE,W116.W11628)                                     
 ORDER W116J028                                                                 
//*                                                                             
//* --- STEG 4. KATALOGISERA FIL MED EV. POSTER KVAR ATT SÄNDA -------          
//*                                                                             
//CATLG    EXEC PGM=IEFBR14                                                     
//CB20XX01 DD  DSN=W116.W116S2.W11683(+1),                                      
//             DISP=(OLD,CATLG,DELETE)                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J028                                         
