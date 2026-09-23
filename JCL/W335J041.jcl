//W335J041 JOB (670W3350100W335J041,W100),'RTN W335D5',                         
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
//W335     EXEC W335P041,                                                       
//             INDIN=W335.W335D5,                                               
//             INDUT=W335.W335D5                                                
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA VCOM --------------------          
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W335Z2M0                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33540V,DISP=SHR                           
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P041 ATT SÄNDA VIA VCOM                       
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W335Z2M1                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33540V,DISP=SHR                           
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P041 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W335Z2M2                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33540V,DISP=SHR                           
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P041 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W335Z2M3                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33540V,DISP=SHR                           
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P041 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W335Z2M4                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33540V,DISP=SHR                           
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P041 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W335Z2M6                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335D5.W33540V,DISP=SHR                           
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P041 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W33541 -------          
//*                                                                             
//SOPBEST EXEC WSOP,COND=(8,NE,W335.W33541)                                     
 ORDER W335J041                                                                 
//*                                                                             
//*                                                                             
//* --- STEG 4. KATALOGISERA FIL MED EV. POSTER KVAR ATT SÄNDA -------          
//*                                                                             
//CATLG    EXEC PGM=IEFBR14                                                     
//CB20XX01 DD  DSN=W335.W335D5.W33540(+1),                                      
//             DISP=(OLD,CATLG,DELETE)                                          
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J041                                         
