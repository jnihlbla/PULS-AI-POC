//W335J042 JOB (670W3350100W335J042,W100),'RTN W335D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//********************************************************************          
//*         VCOM-ÖVERFÖRING AV ARTIKELINFO MB                        *          
//********************************************************************          
//*                                                                             
//* ---------- STEG 1. SKAPA EN 'DEL-FIL' ATT SKICKA -----------------          
//*                                                                             
//*                                                                             
//W335     EXEC W335P042,                                                       
//             INDIN=W335.W335D5,                                               
//             INDUT=W335.W335D5                                                
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA VCOM --------------------          
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W335Z2M5                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33542V(+1),DISP=SHR                       
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P042 ATT SÄNDA VIA VCOM                       
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W33542 -------          
//*                                                                             
//SOPBEST EXEC WSOP,COND=(8,NE,W335.W33542)                                     
 ORDER W335J042                                                                 
//*                                                                             
//*                                                                             
//* --- STEG 4. KATALOGISERA FIL MED EV. POSTER KVAR ATT SÄNDA -------          
//*                                                                             
//CATLG    EXEC PGM=IEFBR14                                                     
//CB20XX01 DD  DSN=W335.W335D5.W33544(+1),                                      
//             DISP=(OLD,CATLG,DELETE)                                          
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J042                                         
