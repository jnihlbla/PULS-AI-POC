//W475JDHL JOB (510W4750100W475JDHL,W100),'RTN W475B2',                         
//             CLASS=N,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=00                                                  
/*ROUTE XEQ NJESD                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*                                                                             
//       EXEC W001HFSG,                                                         
//             CONV='(BPXFX311)',                                               
//             PATHIN='/app/vccs/deve/common/tmp/nordic.esd',                   
//             DSOUT=W475.W475B2.DHLKODER(+1),                                  
//             RECFM=FB,LRECL=29                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W475JDHL                                         
