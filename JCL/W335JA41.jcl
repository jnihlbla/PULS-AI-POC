//W335JA41 JOB (670W3350100W335JA41,W100),'RTN W335D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//********************************************************************          
//*         VCOM-ÖVERFÖRING AV ARTIKELINFO MB                        *          
//********************************************************************          
//*                                                                             
//* ---------- STEG 1. SKAPA EN 'DEL-FIL' ATT SKICKA -----------------          
//*                                                                             
//W335     EXEC W335PA41,                                                       
//             INDIN=W335.W335D7,                                               
//             INDUT=W335.W335D7                                                
//*                                                                             
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA VCOM --------------------          
//*                                                                             
//*************  VCAS                                                           
//*VCOM     EXEC W016P022,VCOM=W335Z2TT                                         
//VCOM     EXEC W016P022,VCOM=W335Z2M1                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D7.W3354VA,DISP=SHR                           
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W33541 -------          
//*                                                                             
//SOPBEST EXEC WSOP,COND=(8,NE,W335.W33541)                                     
 ORDER W335JA41                                                                 
//*                                                                             
//* --- STEG 4. KATALOGISERA FIL MED EV. POSTER KVAR ATT SÄNDA -------          
//*                                                                             
//CATLG    EXEC PGM=IEFBR14                                                     
//CB20XX01 DD  DSN=W335.W335D7.W3354BA(+1),                                     
//             DISP=(OLD,CATLG,DELETE)                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335JA41                                         
