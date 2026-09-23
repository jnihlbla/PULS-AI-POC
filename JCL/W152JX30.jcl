//W152JX30 JOB (640W1520100W152JX30,W100),'RTN W152V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*  THIS JOB CREATES AN INSTANT EXTRACT OF DESCR.DATABASE                      
//*  TO BE USED IN THE FOLLOWING JOB IN THE SAME ROUTINE                        
//*                                                                             
//W015P08X EXEC W015P08X,                                                       
//             DSOUT='W152.W152V1.WDD3XTR(+1)',                                 
//             PSB=WDD3BL                                                       
//SYSIN    DD *                                                                 
****                                                                            
00                                                                              
**                                                                              
**                                                                              
//WDD3V      DD  DSN=WG01.QASE.WDD3V,DISP=SHR                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W152JX30                                         
