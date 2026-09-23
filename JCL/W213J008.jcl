//W213J008 JOB (670W2130100W213J008,W100),'RTN W213V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W015P08X EXEC W015P08X,                                                       
//             DSOUT='W213.W213V1.W213F1(+1)',                                  
//**** DL1PR FILE WITH WDF106+WDF107 SEGM TO NEXT STEP W213P008                 
//             PSB=WDF1BL                                                       
//SYSIN    DD *                                                                 
06  07****                                                                      
00                                                                              
**                                                                              
//WDF1V      DD  DSN=WG01.QASE.WDF1V,DISP=SHR                                   
//*                                                                             
//W213    EXEC W213P008                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W213.W213V1.W21308(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W213.W213V1.W21308(+1)                                    
//SYSIN           DD *                                                          
W21308-001                                                                      
W21308                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J008                                         
