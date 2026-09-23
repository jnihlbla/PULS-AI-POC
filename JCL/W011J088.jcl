//W011J088 JOB (640W0110100W011J088,W100),'RTN W011D6',                         
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
//             DSOUT='W011.W011D6.W01188(+1)',                                  
//             PSB=WDQ1BL                                                       
//SYSIN    DD *                                                                 
****                                                                            
00                                                                              
**                                                                              
**                                                                              
//WDQ1K      DD  DSN=WG01.QASE.WDQ1K,DISP=SHR                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J088                                         
